
#include <stdio.h>

#include "consoler.h"

// Application with a box you can move around the screen.
#define WIDTH 640
#define HEIGHT 480

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
    CNSL_Display display = CNSL_AllocDisplay(WIDTH, HEIGHT);

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

            fill(display, x, y, 10, 10, CNSL_MakeColor(0, 255, 0));
            CNSL_SendDisplay(stdcon, display, 0, 0, 0, 0, 640, 480);
        }
    }

    CNSL_Quit();
    return 0;
}

