
#include <stdio.h>

#include "consoler.h"

void fill(CNSL_Display dpy,
        unsigned int x, unsigned int y,
        unsigned int w, unsigned int h,
        CNSL_Color col)
{
    int c;
    int r;
    for (r = 0; r < h; r++) {
        for (c = 0; c < w; c++) {
            CNSL_SetPixel(dpy, c+x, r+y, col);
        }
    }
}

int main()
{
    CNSL_Init();

    int width = 640;
    int height = 480;
    CNSL_GetGeometry(&width, &height);
    CNSL_Display display = CNSL_AllocDisplay(width, height);

    CNSL_Event event;
    int done = 0;
    int x = 100;
    int y = 100;
    while (!done) {
        CNSL_RecvEvent(stdcon, &event);
        int sym; 
        if (CNSL_IsKeypress(&event, &sym)) {
            fill(display, x, y, 10, 10, CNSL_MakeColor(255, 0, 0));

            switch (sym) {
                case 'q': done = 1; break;
                case 'h': x -= 10; break;
                case 'j': y += 10; break;
                case 'k': y -= 10; break;
                case 'l': x += 10; break;
            }

            fill(display, x, y, 10, 10, 0x00FFFFFF);

            CNSL_SendDisplay(stdcon, display, x-10, y-10, x-10, y-10, 30, 30);
        }
    }

    CNSL_Quit();
    return 0;
}

