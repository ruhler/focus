
#include <stdio.h>

#include "consoler.h"

int main()
{
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

