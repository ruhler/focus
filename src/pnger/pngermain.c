
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

#include "pnger.h"

int main(int argc, char* argv[])
{
    if (argc > 1 && strcmp(argv[1], "--version") == 0) {
        printf("pnger %s\n", FOCUS_VERSION_STRING);
        return 0;
    }

    if (argc > 1 && strcmp(argv[1], "--help") == 0) {
        printf("Usage: pnger FILE\n");
        printf("View the png FILE\n");
        printf("\n");
        printf("Options\n");
        printf("  --help       output this help message and exit\n");
        printf("  --version    output version information and exit\n");
        printf("\n");
        return 0;
    }

    if (argc < 2) {
        fprintf(stderr, "no input file\n");
        return 1;
    }

    char* pngfilename = argv[1];
    Pnger pnger = Pnger_Create(pngfilename);
    if (!pnger) {
        fprintf(stderr, "unable to open %s\n", pngfilename);
        return 1;
    }

    int width;
    int height;

    CNSL_Event event = CNSL_RecvEvent(stdcon);
    if (!CNSL_IsResize(event, &width, &height)) {
        fprintf(stderr, "pnger: expected resize event. Got %i\n", event.type);
        return 1;
    }

    CNSL_Display display = CNSL_AllocDisplay(width, height);
    Pnger_Show(pnger, display);
    CNSL_SendDisplay(stdcon, display, 0, 0, 0, 0, width, height);

    // Wait for q key press to finish.
    bool done = false;

    while (!done) {
        event = CNSL_RecvEvent(stdcon);
        CNSL_Keysym sym;

        if (CNSL_IsKeypress(event, &sym)) {
            switch (sym) {
                case CNSLK_q: done = true; break;

                case CNSLK_h: Pnger_Scroll(pnger, width/10, 0); break;
                case CNSLK_j: Pnger_Scroll(pnger, 0, -height/10); break;
                case CNSLK_k: Pnger_Scroll(pnger, 0, height/10); break;
                case CNSLK_l: Pnger_Scroll(pnger, -width/10, 0); break;

                case CNSLK_i: Pnger_Zoom(pnger, -1); break;
                case CNSLK_o: Pnger_Zoom(pnger, 1); break;
            }

            Pnger_Show(pnger, display);
            CNSL_SendDisplay(stdcon, display, 0, 0, 0, 0, width, height);

        } else if (CNSL_IsQuit(event)) {
            done = true;
        }
    }

    Pnger_Destroy(pnger);
    return 0;
}
