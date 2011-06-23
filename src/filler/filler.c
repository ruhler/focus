
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

    int width = 640;
    int height = 480;
    CNSL_GetGeometry(&width, &height);

    CNSL_Display display = CNSL_AllocDisplay(width, height);
    CNSL_Event event;
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

    CNSL_Color color = black;

    while (!done) {
        int x, y;

        // Fill the screen with the current color.
        for (y = 0; y < height; y++) {
            for (x = 0; x < width; x++) {
                CNSL_SetPixel(display, x, y, color);
            }
        }
        CNSL_SendDisplay(stdcon, display, 0, 0, 0, 0, width, height);

        // Get the next color.
        event = CNSL_RecvEvent(stdcon);
        if (CNSL_IsQuit(event)) {
            done = true;
        } else if (CNSL_IsKeypress(event, &sym)) {
            switch (sym) {
                case CNSLK_r: color = red; break;
                case CNSLK_g: color = green; break;
                case CNSLK_b: color = blue; break;
                case CNSLK_c: color = cyan; break;
                case CNSLK_y: color = yellow; break;
                case CNSLK_p: color = purple; break;
                case CNSLK_w: color = white; break;
                case CNSLK_n: color = black; break;
                case CNSLK_q: done = true; break;
            }
        }
    }

    return 0;
}

