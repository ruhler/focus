
#include <cassert>
#include <cstdlib>
#include <iostream>
#include <string>
#include <sstream>

#include "pdfer.h"

extern "C" {
#include "fonter.h"
}

int main(int argc, char* argv[])
{
    if (argc < 2) {
        std::cerr << "no input file" << std::endl;
        return 1;
    }

    std::string pdffilename = argv[1];

    int width = 640;
    int height = 480;
    int acc = 0;

    CNSL_Init();
    CNSL_GetGeometry(&width, &height);

    FNTR_Fonter fonter = FNTR_Create("Monospace-24:Bold");

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
    bool showstatus = false;

    while (!done) {
        CNSL_RecvEvent(stdcon, &event);
        int sym;

        if (CNSL_IsKeypress(event, &sym)) {
            switch (sym) {
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
                case CNSLK_j: pdfer->scroll(0, -0.1); break;
                case CNSLK_k: pdfer->scroll(0, 0.1); break;
                case CNSLK_h: pdfer->scroll(0.1, 0); break;
                case CNSLK_l: pdfer->scroll(-0.1, 0); break;
                case CNSLK_i: pdfer->zoom(0.8); break;
                case CNSLK_o: pdfer->zoom(1.25); break;
                case CNSLK_w: pdfer->fitwidth(); break;
                case CNSLK_a: pdfer->fitpage(); break;

                case CNSLK_v: showstatus = true; break;
            }

            pdfer->show(display);

            if (showstatus) {
                // Draw the status bar.
                CNSL_Color fg = CNSL_MakeColor(0xFF, 0xFF, 0xFF);
                CNSL_Color bg = CNSL_MakeColor(0x00, 0x00, 0x80);
                std::ostringstream oss;
                oss << pdffilename << "    " << pdfer->page() << " of " << pdfer->pages() << " ";
                FNTR_DrawString(fonter, display, fg, bg, 0, height - FNTR_MaxHeight(fonter), oss.str().c_str());
                showstatus = false;
            }
            CNSL_SendDisplay(stdcon, display, 0, 0, 0, 0, width, height);
        }

    }

    Pdfer::unload(pdfer);

    CNSL_Quit();
    return 0;
}

