
// Copyright (C) 2011 Richard Uhler <ruhler@member.fsf.org>
//
// This file is part of Focus.
//
// Focus is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.
//
// Focus is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with Focus.  If not, see <http://www.gnu.org/licenses/>.

#include <pthread.h>
#include <stdlib.h>
#include <stdio.h>

#include "client.h"
#include "display.h"
#include "inputter.h"
#include "outputter.h"
#include "screen.h"

typedef struct {
    bool pending;
    int width;
    int height;
    pthread_mutex_t lock;
} ResizeInfo_t;


SCREEN_Screen scr;
SCREEN_Position oldcursor;
char* fromclientptr = NULL;
DISPLAY_Display display;
CLIENT_Client client;
ResizeInfo_t resize;

void drawcell(SCREEN_Position pos, const SCREEN_Cell* cell)
{
    DISPLAY_DrawCell(display, pos, cell);
}

SCREEN_Cell curserify(SCREEN_Cell c)
{
    c.cattrs.fgcolor = SCREEN_COLOR_BLACK;
    c.cattrs.bgcolor = SCREEN_COLOR_WHITE;
    return c;
}

char getf()
{
    if (*fromclientptr == '\0') {
        // Check for a resize event and handle if needed.
        pthread_mutex_lock(&resize.lock);
        if (resize.pending) {
            resize.pending = false;
            DISPLAY_Resize(display, resize.width, resize.height);
            CLIENT_Resize(client, DISPLAY_Columns(display), DISPLAY_Lines(display));
            SCREEN_Resize(&scr, DISPLAY_Columns(display), DISPLAY_Lines(display));
        }
        pthread_mutex_unlock(&resize.lock);

        // Update the screen.
        diff(&scr, drawcell);
        SCREEN_Cell cell = cellat(&scr, oldcursor);
        drawcell(oldcursor, &cell);

        oldcursor = cursor(&scr);
        cell = curserify(cellat(&scr, oldcursor));
        drawcell(oldcursor, &cell);
        DISPLAY_Show(display);

        // Get more input.
        fromclientptr = CLIENT_Read(client);
    }

    char c = *fromclientptr;
    fromclientptr++;
    return c;
}

CNSL_Event getevent()
{
    int width, height;
    CNSL_Event event = CNSL_RecvEvent(stdcon);
    while (CNSL_IsResize(event, &width, &height)) {
        pthread_mutex_lock(&resize.lock);
            resize.pending = true;
            resize.width = width;
            resize.height = height;
        pthread_mutex_unlock(&resize.lock);

        event = CNSL_RecvEvent(stdcon);
    }
    return event;
}
    

void* runoutputter(void* ud)
{
    fromclientptr = CLIENT_Read(client);
    oldcursor = mkpos(0, 0);
    outputter(&scr, '\0', getf);
    exit(0);
}

void iput(char c)
{
    CLIENT_Write(client, c);
}
    

int main(int argc, char* argv[])
{
    if (argc > 1 && strcmp(argv[1], "--version") == 0) {
        printf("termer %s\n", FOCUS_VERSION_STRING);
        return 0;
    }

    if (argc > 1 && strcmp(argv[1], "--help") == 0) {
        printf("Usage: termer\n");
        printf("A consoler terminal emulator\n");
        printf("\n");
        printf("Options\n");
        printf("  --help       output this help message and exit\n");
        printf("  --version    output version information and exit\n");
        printf("\n");
        return 0;
    }
    
    display = DISPLAY_Alloc();

    char colsstr[10] = {0};
    char linesstr[10] = {0};
    snprintf(colsstr, 10, "%i", DISPLAY_Columns(display));
    snprintf(linesstr, 10, "%i", DISPLAY_Lines(display));
    setenv("COLUMNS", colsstr, 1);
    setenv("LINES", linesstr, 1);

    client = CLIENT_Open();
    CLIENT_Resize(client, DISPLAY_Columns(display), DISPLAY_Lines(display));
    scr = screen(DISPLAY_Columns(display), DISPLAY_Lines(display));

    resize.pending = false;
    pthread_mutex_init(&resize.lock, NULL);

    pthread_t othread;
    pthread_create(&othread, NULL, &runoutputter, NULL);
    inputter(getevent, iput);
    
    CLIENT_Close(client);
    DISPLAY_Free(display);
    return 0;
}

