
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

#include <assert.h>
#include <poll.h>
#include <stdlib.h>
#include <stdio.h>
#include <unistd.h>

#include "consoler.h"

const char* CNSL_Version()
{
    return FOCUS_VERSION_STRING;
}

// Event types
#define CNSLE_KEYPRESS 0
#define CNSLE_KEYRELEASE 1
#define CNSLE_QUIT 2
#define CNSLE_RESIZE 3

CNSL_Event CNSL_MakeKeypress(CNSL_Keysym sym)
{
    CNSL_Event event;
    event.type = CNSLE_KEYPRESS;
    event.v1 = sym;
    event.v2 = 0;
    return event;
}

CNSL_Event CNSL_MakeKeyrelease(CNSL_Keysym sym)
{
    CNSL_Event event;
    event.type = CNSLE_KEYRELEASE;
    event.v1 = sym;
    event.v2 = 0;
    return event;
}

CNSL_Event CNSL_MakeQuit()
{
    CNSL_Event event;
    event.type = CNSLE_QUIT;
    event.v1 = 0;
    event.v2 = 0;
    return event;
}

CNSL_Event CNSL_MakeResize(int width, int height)
{
    CNSL_Event event;
    event.type = CNSLE_RESIZE;
    event.v1 = width;
    event.v2 = height;
    return event;
}

bool CNSL_IsKeypress(CNSL_Event event, CNSL_Keysym* sym)
{
    if (event.type == CNSLE_KEYPRESS) {
        *sym = event.v1;
    }
    return event.type == CNSLE_KEYPRESS;
}

bool CNSL_IsKeyrelease(CNSL_Event event, CNSL_Keysym* sym)
{
    if (event.type == CNSLE_KEYRELEASE) {
        *sym = event.v1;
    }
    return event.type == CNSLE_KEYRELEASE;
}

bool CNSL_IsQuit(CNSL_Event event)
{
    return event.type == CNSLE_QUIT;
}

bool CNSL_IsResize(CNSL_Event event, int* width, int* height)
{
    if (event.type == CNSLE_RESIZE) {
        *width = event.v1;
        *height = event.v2;
        return true;
    }
    return false;
}

bool CNSL_EventsEqual(CNSL_Event a, CNSL_Event b)
{
    if (a.type != b.type) {
        return false;
    }

    switch (a.type) {
        case CNSLE_QUIT:
            return true;

        case CNSLE_KEYPRESS:
        case CNSLE_KEYRELEASE:
            return a.v1 == b.v1;

        case CNSLE_RESIZE:
            return a.v1 == b.v1 && a.v2 == b.v2;

        default:
            assert(false && "invalid event type in equality check");
    }
    return false;
}

uint8_t CNSL_GetRed8(CNSL_Color c)
{
    return 0xFF & (c >> 16);
}

uint8_t CNSL_GetBlue8(CNSL_Color c)
{
    return 0xFF & c;
}

uint8_t CNSL_GetGreen8(CNSL_Color c)
{
    return 0xFF & (c >> 8);
}

uint32_t CNSL_GetRGB8(CNSL_Color c)
{
    return c;
}

CNSL_Color CNSL_MakeColor(uint8_t r, uint8_t g, uint8_t b)
{
    return (r << 16) | (g << 8) | b;
}


CNSL_Display CNSL_AllocDisplay(unsigned int width, unsigned int height) {
    CNSL_Display display;
    display.width = width;
    display.height = height;
    display.pixels = malloc(sizeof(CNSL_Color) * width * height);
    return display;
}

void CNSL_FreeDisplay(CNSL_Display display)
{
    free(display.pixels);
}

CNSL_Color CNSL_GetPixel(CNSL_Display display, unsigned int x, unsigned int y)
{
    return display.pixels[y*display.width + x];
}

void CNSL_SetPixel(CNSL_Display display, unsigned int x, unsigned int y, CNSL_Color color)
{
    display.pixels[y*display.width + x] = color;
}

void CNSL_FillRect(CNSL_Display display, unsigned int x, unsigned int y, unsigned int w, unsigned int h, CNSL_Color color)
{
    CNSL_Color* top = display.pixels + y*display.width;
    CNSL_Color* bottom = top + h*display.width;
    CNSL_Color* row;
    for (row = top; row < bottom; row += display.width) {
        CNSL_Color* left = row + x;
        CNSL_Color* right = left + w;
        CNSL_Color* ptr = left + w;
        for (ptr = left; ptr < right; ptr++) {
            *ptr = color;
        }
    }
}

CNSL_Console stdcon = { STDIN_FILENO, STDOUT_FILENO };

CNSL_Client CNSL_LaunchClient(const char* path, char* const args[])
{
    CNSL_Client client = {-1, -1};
    int stoc[2];
    int ctos[2];

    if (pipe(stoc) < 0) {
        perror("pipe");
        return client;
    }

    if (pipe(ctos) < 0) {
        perror("pipe");
        return client;
    }

    pid_t pid = fork();
    if (pid < 0) {
        perror("fork");
        return client;
    }

    if (pid == 0) {
        if (dup2(stoc[0], STDIN_FILENO) < 0) {
            perror("dup2");
            return client;
        }
        close(stoc[0]);
        close(stoc[1]);

        if (dup2(ctos[1], STDOUT_FILENO) < 0) {
            perror("dup2");
            return client;
        }
        close(ctos[0]);
        close(ctos[1]);

        if (execvp(path, args) < 0) {
            perror("execvp");
            return client;
        }

    }

    close(stoc[0]);
    close(ctos[1]);

    client.fdout = stoc[1];
    client.fdin = ctos[0];
    return client;
}

void CNSL_CloseClient(CNSL_Client client)
{
    close(client.fdin);
    close(client.fdout);
}

// Just like the read system call, only always reads as many bytes as
// specified if possible before returning.
// Returns either -1 (on error), 0 (on EOF), or positive (on success).
int read_all(int fd, void* buf, size_t count)
{
    int red = read(fd, buf, count);
    if (red > 0 && red < count) {
        return read_all(fd, buf+red, count-red);
    }
    return red;

}

// Just like the write system call, only always write as many bytes as
// specified if possible before returning.
// Returns either -1 (on error), 0 (on EOF), or positive (on success).
int write_all(int fd, const void* buf, size_t count)
{
    int written = write(fd, buf, count);
    if (written > 0 && written < count) {
        return write_all(fd, buf+written, count-written);
    }
    return written;

}

CNSL_Event CNSL_RecvEvent(CNSL_Console console)
{
    CNSL_Event event = CNSL_MakeQuit();
    read_all(console.fdin, &event, sizeof(CNSL_Event));
    return event;
}

bool CNSL_SendEvent(CNSL_Client client, CNSL_Event event)
{
    struct pollfd fd;
    fd.fd = client.fdout;
    fd.events = POLLOUT;
    if (poll(&fd, 1, -1) == 1) {
        if (fd.revents & (POLLERR | POLLHUP | POLLNVAL)) {
            return false;
        } else if (fd.revents & POLLOUT) {
            int wrote = write_all(client.fdout, &event, sizeof(CNSL_Event));
            return wrote > 0;
        }
    }
    return false;
}

void CNSL_SendDisplay(CNSL_Console console, CNSL_Display display,
        unsigned int srcx, unsigned int srcy,
        unsigned int dstx, unsigned int dsty,
        unsigned int width, unsigned int height)
{
    assert(srcx < display.width);
    assert(srcy < display.height);
    assert(srcx + width <= display.width);
    assert(srcy + height <= display.height);

    // Output the data using the following format:
    // ui4: x destination
    // ui4: y destination
    // ui4: width
    // ui4: height
    // width*ui4: row 1
    // width*ui4: row 2
    // ...
    // width*ui4: row (height)

    unsigned int header[] = {dstx, dsty, width, height};
    write_all(console.fdout, header, 4*sizeof(unsigned int));

    int y;
    for (y = srcy; y < height+srcy; y++) {
        CNSL_Color* line = display.pixels + (y * display.width) + srcx;
        write_all(console.fdout, line, width*sizeof(CNSL_Color));
    }
}

bool CNSL_RecvDisplay(CNSL_Client client, CNSL_Display display,
        unsigned int* dstx_out, unsigned int* dsty_out,
        unsigned int* width_out, unsigned int* height_out)
{
    static CNSL_Color* junk = NULL;
    static junkwidth = 0;

    unsigned int header[4] = {0};
    ssize_t red = read_all(client.fdin, header, 4 * sizeof(unsigned int));
    if (red <= 0) {
        return false;
    }

    unsigned int dstx = header[0];
    unsigned int dsty = header[1];
    unsigned int width = header[2];
    unsigned int height = header[3];

    if (width > junkwidth) {
        // We introduce a small memory leak here. I think it's okay.
        junk = realloc(junk, width * sizeof(CNSL_Color));
        junkwidth = width;
    }

    int wmax = display.width - dstx;
    int wuse = wmax < width ? wmax : width;
    int wleft = width - wuse;

    int y;
    for (y = dsty; y < dsty + height && y < display.height; y++) {
        CNSL_Color* buf = display.pixels + display.width * y + dstx;

        read_all(client.fdin, buf, wuse * sizeof(CNSL_Color));
        read_all(client.fdin, junk, wleft * sizeof(CNSL_Color));
    }
    int huse = y - dsty;

    for (; y < dsty + height; y++) {
        read_all(client.fdin, junk, width * sizeof(CNSL_Color));
    }

    if (dstx_out) {
        *dstx_out = dstx;
    }
    if (dsty_out) {
        *dsty_out = dsty;
    }
    if (width_out) {
        *width_out = wuse;
    }
    if (height_out) {
        *height_out = huse;
    }

    return true;
}

void CNSL_GetGeometry(int* width, int* height)
{
    const char* widthenv = getenv("CNSLWIDTH");
    const char* heightenv = getenv("CNSLHEIGHT");

    if (widthenv) {
        *width = atoi(widthenv);
    }

    if (heightenv) {
        *height = atoi(heightenv);
    }
}

void CNSL_SetGeometry(int width, int height)
{
    char widthstr[10] = {0};
    char heightstr[10] = {0};
    snprintf(widthstr, 10, "%i", width);
    snprintf(heightstr, 10, "%i", height);
    setenv("CNSLWIDTH", widthstr, 1);
    setenv("CNSLHEIGHT", heightstr, 1);
}

