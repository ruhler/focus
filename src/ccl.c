
#include <stdio.h>
#include <stdlib.h>

#include "ccl.h"

void ccl_clear()
{
    // The clear command: C
    printf("C");
    fflush(stdout);
}

int redof(Color c)
{
    return 0xFF & (c >> 16);
}

int greenof(Color c)
{
    return 0xFF & (c >> 8);
}

int blueof(Color c)
{
    return 0xFF & c;
}

void ccl_pixel(int x, int y, Color c)
{
    // The pixel command: P@x,y:RRGGBB
    // x and y are expressed in hex.
    printf("P@%x,%x:%02X.%02X.%02X", x, y, redof(c), greenof(c), blueof(c));
    fflush(stdout);
}

void ccl_fill(int x, int y, int w, int h, Color c)
{
    int i, j;
    for (i = 0; i < w; i++) {
        for (j = 0; j < h; j++) {
            ccl_pixel(x+i, y+j, c);
        }
    }
}

Color ccl_rgb8(int r, int g, int b)
{
    return (r << 16) | (g << 8) | b;
}

void ccl_event(Event* event)
{
    // Event
    // One of:
    // PHH - key press with code in hex
    // RHH - key release with code in hex.
    if (feof(stdin)) {
        fprintf(stderr, "client: got end of file\n");
        exit(0);
    }

    char t;
    scanf("%c%02X", &t, &(event->value));
    fprintf(stderr, "client: got event type: %c(%02X)\n", t, t);
    switch (t) {
        case 'P': event->type = EVENT_KEYPRESS; break;
        case 'R': event->type = EVENT_KEYRELEASE; break;
        default:
            fprintf(stderr, "client: invalid event type in input: %c(%02X)\n", t, t);
            break;
    }
}

int ccl_keypress(const Event* e, int* code)
{
    if (e->type == EVENT_KEYPRESS) {
        *code = e->value;
    }
    return e->type == EVENT_KEYPRESS;
}

int ccl_keyrelease(const Event* e, int* code)
{
    if (e->type == EVENT_KEYRELEASE) {
        *code = e->value;
    }
    return e->type == EVENT_KEYRELEASE;
}

