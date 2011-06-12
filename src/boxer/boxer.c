
#include <stdio.h>

#include "consoler.h"

#define BOX_LENGTH 10

int min(a, b)
{
    return a < b ? a : b;
}

int max(a, b)
{
    return a > b ? a : b;
}

// Draw a box with upper left hand corner at (x,y) of the given color.
void box(CNSL_Display dpy, unsigned int x, unsigned int y, CNSL_Color col)
{
    int c;
    int r;
    for (r = 0; r < BOX_LENGTH; r++) {
        for (c = 0; c < BOX_LENGTH; c++) {
            CNSL_SetPixel(dpy, c+(x*BOX_LENGTH), r+(y*BOX_LENGTH), col);
        }
    }
}

int main()
{
    int width = 640;
    int height = 480;
    CNSL_GetGeometry(&width, &height);
    CNSL_Display display = CNSL_AllocDisplay(width, height);

    int gridwidth = width/BOX_LENGTH;
    int gridheight = height/BOX_LENGTH;

    CNSL_Color red = CNSL_MakeColor(0xff, 0x00, 0x00);
    CNSL_Color white = CNSL_MakeColor(0xff, 0xff, 0xff);

    CNSL_Event event;
    bool done = false;
    int x = 100/BOX_LENGTH;
    int y = 100/BOX_LENGTH;
    box(display, x, y, white);
    CNSL_SendDisplay(stdcon, display, x*BOX_LENGTH, y*BOX_LENGTH,
            x*BOX_LENGTH, y*BOX_LENGTH, BOX_LENGTH, BOX_LENGTH);

    while (!done) {
        event = CNSL_RecvEvent(stdcon);
        CNSL_Keysym sym; 
        if (CNSL_IsKeypress(event, &sym)) {
            int oldx = x;
            int oldy = y;

            box(display, x, y, red);

            switch (sym) {
                case 'q': done = true; break;
                case 'h': x--; break;
                case 'j': y++; break;
                case 'k': y--; break;
                case 'l': x++; break;
            }

            if (x < 0) {
                x += gridwidth;
            } else if (x > gridwidth) {
                x -= gridwidth;
            }

            if (y < 0) {
                y += gridheight;
            } else if (y > gridheight) {
                y -= gridheight;
            }

            box(display, x, y, white);

            int updx = min(oldx, x) * BOX_LENGTH;
            int updy = min(oldy, y) * BOX_LENGTH;
            int updw = (max(oldx, x) - min(oldx, x) + 1) * BOX_LENGTH;
            int updh = (max(oldy, y) - min(oldy, y) + 1) * BOX_LENGTH;;
            CNSL_SendDisplay(stdcon, display, updx, updy, updx, updy, updw, updh);
        } else if (CNSL_IsQuit(event)) {
            done = true;
        }
    }
    return 0;
}

