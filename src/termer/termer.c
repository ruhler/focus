
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

#include "ctermer.h"
#include "inputter.h"
#include "outputter.h"
#include "screen.h"

SCREEN_Screen scr;
SCREEN_Position oldcursor;
char* fromclientptr = NULL;

void drawcell(SCREEN_Position pos, const SCREEN_Cell* cell)
{
    int fgc = cell->cattrs.style.reverse ? cell->cattrs.bgcolor : cell->cattrs.fgcolor;
    int bgc = cell->cattrs.style.reverse ? cell->cattrs.fgcolor : cell->cattrs.bgcolor;
    int style = cell->cattrs.style.bold ? 1 : 0;

    ctermer_DrawCell(pos.column, pos.line, cell->character, style, fgc, bgc);
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
        // Update the screen now, then ask for more input.
        diff(&scr, drawcell);
        SCREEN_Cell cell = cellat(&scr, oldcursor);
        drawcell(oldcursor, &cell);

        oldcursor = cursor(&scr);
        cell = curserify(cellat(&scr, oldcursor));
        drawcell(oldcursor, &cell);
        ctermer_ShowDisplay();

        fromclientptr = ctermer_FromTermClient();
    }

    char c = *fromclientptr;
    fromclientptr++;
    return c;
}

CNSL_Event getevent()
{
    return CNSL_RecvEvent(stdcon);
}
    

void* runoutputter(void* ud)
{
    fromclientptr = ctermer_FromTermClient();
    oldcursor = mkpos(0, 0);
    outputter(&scr, '\0', getf);
    exit(0);
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
    
    int cols, lines;
    ctermer_Init(&cols, &lines);

    scr = screen(cols, lines);

    pthread_t othread;
    pthread_create(&othread, NULL, &runoutputter, NULL);
    inputter(getevent, ctermer_ToTermClient);
    ctermer_DeInit();
    return 0;
}

