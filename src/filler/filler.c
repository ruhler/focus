
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

#include "consoler.h"

void fill(CNSL_Display display, CNSL_Color color)
{
    fprintf(stderr, "filler: send %08x\n", color);
    CNSL_FillRect(display, 0, 0, display.width, display.height, color);
    CNSL_SendDisplay(stdcon, display, 0, 0, 0, 0, display.width, display.height);
}

void change(CNSL_Display display, CNSL_Color color, CNSL_Color* save)
{
    *save = color;
    fill(display, color);
}

int main(int argc, char* argv[])
{
    if (argc > 1 && strcmp(argv[1], "--version") == 0) {
        printf("filler %s\n", FOCUS_VERSION_STRING);
        return 0;
    }

    if (argc > 1 && strcmp(argv[1], "--help") == 0) {
        printf("Usage: filler\n");
        printf("An application to color the screen\n");
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
        fprintf(stderr, "filler: expected recv event.\n");
        return 1;
    }

    CNSL_Display display = CNSL_AllocDisplay(width, height);
    CNSL_Keysym sym;
    bool done = false;

    CNSL_Color black = CNSL_MakeColor(0, 0, 0);
    CNSL_Color red = CNSL_MakeColor(255, 0, 0);
    CNSL_Color green = CNSL_MakeColor(0, 255, 0);
    CNSL_Color blue = CNSL_MakeColor(0, 0, 255);
    CNSL_Color cyan = CNSL_MakeColor(0, 255, 255);
    CNSL_Color yellow = CNSL_MakeColor(255, 255, 0);
    CNSL_Color purple = CNSL_MakeColor(255, 0, 255);
    CNSL_Color white = CNSL_MakeColor(255, 255, 255);

    CNSL_Color color;
    change(display, black, &color);

    while (!done) {
        event = CNSL_RecvEvent(stdcon);
        if (CNSL_IsQuit(event)) {
            done = true;
        } else if (CNSL_IsResize(event, &width, &height)) {
            fprintf(stderr, "filler: resize %i, %i\n", width, height);
            CNSL_FreeDisplay(display);
            display = CNSL_AllocDisplay(width, height);
            fill(display, color);
        } else if (CNSL_IsKeypress(event, &sym)) {
            fprintf(stderr, "filler: keypress: %c(%i)\n", sym, sym);
            switch (sym) {
                case CNSLK_r: change(display, red, &color); break;
                case CNSLK_g: change(display, green, &color); break;
                case CNSLK_b: change(display, blue, &color); break;
                case CNSLK_c: change(display, cyan, &color); break;
                case CNSLK_y: change(display, yellow, &color); break;
                case CNSLK_p: change(display, purple, &color); break;
                case CNSLK_w: change(display, white, &color); break;
                case CNSLK_n: change(display, black, &color); break;
                case CNSLK_q: done = true; break;
            }
        }
    }

    return 0;
}

