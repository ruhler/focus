
#include <stdio.h>

#include "ccl.h"

// Application with a box you can move around the screen.

int main()
{
    ccl_clear();

    Event event;
    int done = 0;
    int x = 100;
    int y = 100;
    while (!done) {
        ccl_event(&event);
        int code; 
        if (ccl_keypress(&event, &code)) {
            fprintf(stderr, "client: got code: %i\n", code);
            switch (code) {
                case 16:   // q
                    done = 1;
                    break;

                case 35:   // h
                    x -= 10;
                    break;

                case 36:   // j
                    y += 10;
                    break;

                case 37:   // k
                    y -= 10;
                    break;

                case 38:   // l
                    x += 10;
                    break;
            }
            ccl_clear();
            ccl_fill(x, y, 10, 10, ccl_rgb8(255, 0, 0));
        }
    }

    return 0;
}

