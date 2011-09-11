
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

#include "filler.h"

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

    int width, height;
    CNSL_Event event = CNSL_RecvEvent(stdcon);
    if (!CNSL_IsResize(event, &width, &height)) {
        fprintf(stderr, "filler: expected resize event. Got %i\n", event.type);
        return 1;
    }

    CNSL_Keysym sym;

    CNSL_Color black = CNSL_MakeColor(0, 0, 0);
    CNSL_Color red = CNSL_MakeColor(255, 0, 0);
    CNSL_Color green = CNSL_MakeColor(0, 255, 0);
    CNSL_Color blue = CNSL_MakeColor(0, 0, 255);
    CNSL_Color cyan = CNSL_MakeColor(0, 255, 255);
    CNSL_Color yellow = CNSL_MakeColor(255, 255, 0);
    CNSL_Color purple = CNSL_MakeColor(255, 0, 255);
    CNSL_Color white = CNSL_MakeColor(255, 255, 255);

    Filler filler = Filler_Create(width, height, black, stdcon);
    bool done = false;

    while (!done) {
        event = CNSL_RecvEvent(stdcon);
        if (CNSL_IsQuit(event)) {
            fprintf(stderr, "filler: quit\n");
            done = true;
        } else if (CNSL_IsResize(event, &width, &height)) {
            fprintf(stderr, "filler: resize %i, %i\n", width, height);
            Filler_Resize(filler, width, height);
        } else if (CNSL_IsKeypress(event, &sym)) {
            fprintf(stderr, "filler: keypress: %c(%i)\n", sym, sym);
            switch (sym) {
                case CNSLK_r: Filler_FillWith(filler, red); break;
                case CNSLK_g: Filler_FillWith(filler, green); break;
                case CNSLK_b: Filler_FillWith(filler, blue); break;
                case CNSLK_c: Filler_FillWith(filler, cyan); break;
                case CNSLK_y: Filler_FillWith(filler, yellow); break;
                case CNSLK_p: Filler_FillWith(filler, purple); break;
                case CNSLK_w: Filler_FillWith(filler, white); break;
                case CNSLK_n: Filler_FillWith(filler, black); break;
                case CNSLK_q: done = true; break;
                case CNSLK_d: 
                    width *= 2;
                    height *= 2;          
                    Filler_Resize(filler, width, height);
                    break;

                case CNSLK_h: 
                    width /= 2;
                    height /= 2;          
                    Filler_Resize(filler, width, height);
                    break;
            }
        }
    }

    Filler_Free(filler);

    return 0;
}
