
#include <stdio.h>

#include "ccl.h"

// Application with a box you can move around the screen.
#define WIDTH 640
#define HEIGHT 480

void fill(Buffer dpy,
        unsigned int x, unsigned int y,
        unsigned int w, unsigned int h,
        Color col)
{
    int c;
    int r;
    for (r = 0; r < h; r++) {
        for (c = 0; c < w; c++) {
            ccl_setpixel(dpy, c+x, r+y, col);
        }
    }
}

int main()
{
    Buffer display = ccl_alloc_buffer(WIDTH, HEIGHT);

    Event event;
    int done = 0;
    int x = 100;
    int y = 100;
    while (!done) {
        ccl_event(&event);
        int code; 
        if (ccl_keypress(&event, &code)) {
            fill(display, x, y, 10, 10, ccl_rgb8(255, 0, 0));
            x += 10;
            y += 10;
            fill(display, x, y, 10, 10, ccl_rgb8(0, 255, 0));
            ccl_blit(display, 0, 0, 0, 0, 640, 480);
        }
    }

    return 0;
}

