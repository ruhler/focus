
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

#include <iostream>

#include "pdfer.h"

double zoomamt(bool shifton, bool ctrlon)
{
    if (ctrlon) {
        return 9.0/10.0;
    }
    if (shifton) {
        return 2.0/3.0;
    }
    return 4.0/5.0;
}

int main(int argc, char* argv[])
{
    if (argc > 1 && strcmp(argv[1], "--version") == 0) {
        printf("pdfer %s\n", FOCUS_VERSION_STRING);
        return 0;
    }

    if (argc > 1 && strcmp(argv[1], "--help") == 0) {
        printf("Usage: pdfer FILE\n");
        printf("View the pdf FILE\n");
        printf("\n");
        printf("Options\n");
        printf("  --help       output this help message and exit\n");
        printf("  --version    output version information and exit\n");
        printf("\n");
        return 0;
    }

    if (argc < 2) {
        std::cerr << "no input file" << std::endl;
        return 1;
    }

    std::string pdffilename = argv[1];

    int width = 640;
    int height = 480;
    CNSL_GetGeometry(&width, &height);

    Pdfer* pdfer = Pdfer::load(pdffilename, width, height);
    if (!pdfer) {
        std::cerr << "Error loading pdf " << pdffilename << std::endl;
        return 1;
    }

    CNSL_Display display = CNSL_AllocDisplay(width, height);
    pdfer->show(display);
    CNSL_SendDisplay(stdcon, display, 0, 0, 0, 0, width, height);

    CNSL_Event event;
    bool done = false;
    bool shifton = false;
    bool ctrlon = false;
    int acc = 0;

    while (!done) {
        event = CNSL_RecvEvent(stdcon);
        CNSL_Keysym sym;

        if (CNSL_IsKeypress(event, &sym)) {
            pdfer->unstatus();
            switch (sym) {
                case CNSLK_LSHIFT: shifton = true; break;
                case CNSLK_RSHIFT: shifton = true; break;
                case CNSLK_LCTRL: ctrlon = true; break;
                case CNSLK_RCTRL: ctrlon = true; break;

                case CNSLK_q: done = true; break;

                case CNSLK_SPACE:
                case CNSLK_PAGEDOWN:
                case CNSLK_n: pdfer->next(); break;

                case CNSLK_PAGEUP:
                case CNSLK_p: pdfer->previous(); break;

                case CNSLK_END: pdfer->last(); break;
                case CNSLK_HOME: pdfer->first(); break;

                // Digits add to an accumulator
                case CNSLK_0: case CNSLK_1: case CNSLK_2:
                case CNSLK_3: case CNSLK_4: case CNSLK_5:
                case CNSLK_6: case CNSLK_7: case CNSLK_8:
                case CNSLK_9: acc = acc * 10 + (sym - CNSLK_0); break;

                // c clears the accumulator
                case CNSLK_c: acc = 0; break;
                case CNSLK_g: pdfer->goto_(acc); acc = 0; break;

                case CNSLK_h: pdfer->scroll(ctrlon ? .01 : 0.1, 0); break;
                case CNSLK_j: pdfer->scroll(0, ctrlon ? -.01 : -0.1); break;
                case CNSLK_k: pdfer->scroll(0, ctrlon ? .01 : 0.1); break;
                case CNSLK_l: pdfer->scroll(ctrlon ? -.01 : -0.1, 0); break;

                case CNSLK_d: pdfer->scroll(0, -.5); break;
                case CNSLK_u: pdfer->scroll(0, .5); break;
                case CNSLK_f: pdfer->scroll(0, -.9); break;
                case CNSLK_b: pdfer->scroll(0, .9); break;
                case CNSLK_t: pdfer->top(); break;
                case CNSLK_e: pdfer->bottom(); break;

                case CNSLK_i: pdfer->zoom(zoomamt(shifton, ctrlon)); break;
                case CNSLK_o: pdfer->zoom(1.0/zoomamt(shifton, ctrlon)); break;

                case CNSLK_w: pdfer->fitwidth(); break;
                case CNSLK_a: pdfer->fitpage(); break;

                case CNSLK_v: pdfer->status(); break;
            }

            pdfer->show(display);

            CNSL_SendDisplay(stdcon, display, 0, 0, 0, 0, width, height);
        } else if (CNSL_IsKeyrelease(event, &sym)) {
            switch (sym) {
                case CNSLK_LSHIFT: shifton = false; break;
                case CNSLK_RSHIFT: shifton = false; break;
                case CNSLK_LCTRL: ctrlon = false; break;
                case CNSLK_RCTRL: ctrlon = false; break;
            }
        }
    }

    Pdfer::unload(pdfer);
    return 0;
}

