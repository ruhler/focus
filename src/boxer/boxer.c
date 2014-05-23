
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

#include <stdio.h>

#include "config.h"
#include "consoler.h"

#define BOX_LENGTH 10

int min(a, b)
{
    return a < b ? a : b;
}

int max(a, b)
{
    return a > b ? a : b;
}

// Draw a box with upper left hand corner at (x,y) of the given color.
void box(CNSL_Display dpy, unsigned int x, unsigned int y, CNSL_Color col)
{
    int c;
    int r;
    for (r = 0; r < BOX_LENGTH; r++) {
        for (c = 0; c < BOX_LENGTH; c++) {
            CNSL_SetPixel(dpy, c+(x*BOX_LENGTH), r+(y*BOX_LENGTH), col);
        }
    }
}

int main(int argc, char* argv[])
{
    if (argc > 1 && strcmp(argv[1], "--version") == 0) {
        printf("boxer %s\n", PACKAGE_VERSION);
        return 0;
    }

    if (argc > 1 && strcmp(argv[1], "--help") == 0) {
        printf("Usage: boxer\n");
        printf("A game with a box\n");
        printf("\n");
        printf("Options\n");
        printf("  --help       output this help message and exit\n");
        printf("  --version    output version information and exit\n");
        printf("\n");
        return 0;
    }

    int width;
    int height;

    CNSL_Event event = CNSL_RecvEvent(stdcon);
    if (!CNSL_IsResize(event, &width, &height)) {
        fprintf(stderr, "boxer: expected resize event. Got %i\n", event.type);
        return 1;
    }

    CNSL_Display display = CNSL_AllocDisplay(width, height);

    int gridwidth = width/BOX_LENGTH;
    int gridheight = height/BOX_LENGTH;

    CNSL_Color red = CNSL_MakeColor(0xff, 0x00, 0x00);
    CNSL_Color white = CNSL_MakeColor(0xff, 0xff, 0xff);

    bool done = false;
    int x = 100/BOX_LENGTH;
    int y = 100/BOX_LENGTH;
    box(display, x, y, white);
    CNSL_SendDisplay(stdcon, display, x*BOX_LENGTH, y*BOX_LENGTH,
            x*BOX_LENGTH, y*BOX_LENGTH, BOX_LENGTH, BOX_LENGTH);

    while (!done) {
        event = CNSL_RecvEvent(stdcon);
        CNSL_Keysym sym; 
        if (CNSL_IsKeypress(event, &sym)) {
            int oldx = x;
            int oldy = y;

            box(display, x, y, red);

            switch (sym) {
                case 'q': done = true; break;
                case 'h': x--; break;
                case 'j': y++; break;
                case 'k': y--; break;
                case 'l': x++; break;
            }

            if (x < 0) {
                x += gridwidth;
            } else if (x > gridwidth) {
                x -= gridwidth;
            }

            if (y < 0) {
                y += gridheight;
            } else if (y > gridheight) {
                y -= gridheight;
            }

            box(display, x, y, white);

            int updx = min(oldx, x) * BOX_LENGTH;
            int updy = min(oldy, y) * BOX_LENGTH;
            int updw = (max(oldx, x) - min(oldx, x) + 1) * BOX_LENGTH;
            int updh = (max(oldy, y) - min(oldy, y) + 1) * BOX_LENGTH;;
            CNSL_SendDisplay(stdcon, display, updx, updy, updx, updy, updw, updh);
        } else if (CNSL_IsQuit(event)) {
            done = true;
        }
    }
    return 0;
}

