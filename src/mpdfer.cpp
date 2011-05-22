
#include <cassert>
#include <cstdlib>
#include <iostream>
#include <string>

#include "pdfer.h"

int main(int argc, char* argv[])
{
    if (argc < 2) {
        std::cerr << "no input file" << std::endl;
        return 1;
    }

    std::string pdffilename = argv[1];

    int width = 640;
    int height = 480;

    CNSL_Init();
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
    
    while (!done) {
        CNSL_RecvEvent(stdcon, &event);
        int sym;

        if (CNSL_IsKeypress(&event, &sym)) {
            switch (sym) {
                case CNSLK_q: done = true; break;
                case CNSLK_SPACE:
                case CNSLK_n: pdfer->next(); break;
                case CNSLK_p: pdfer->previous(); break;
                case CNSLK_j: pdfer->scroll(0, -0.1); break;
                case CNSLK_k: pdfer->scroll(0, 0.1); break;
                case CNSLK_h: pdfer->scroll(0.1, 0); break;
                case CNSLK_l: pdfer->scroll(-0.1, 0); break;
                case CNSLK_i: pdfer->zoom(0.8); break;
                case CNSLK_o: pdfer->zoom(1.25); break;
                case CNSLK_w: pdfer->fitwidth(); break;
                case CNSLK_a: pdfer->fitpage(); break;
            }

            pdfer->show(display);
            CNSL_SendDisplay(stdcon, display, 0, 0, 0, 0, width, height);
        }

    }

    Pdfer::unload(pdfer);

    CNSL_Quit();
    return 0;
}

