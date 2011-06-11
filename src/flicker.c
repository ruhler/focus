
#include <stdio.h>

#include "consoler.h"

// Application which flickers the screen from black to white a bank
// repeatedly.
//
// This is a benchmark of the performance of the communication link and the
// consoler server.

int main(int argc, char* argv[])
{
    if (argc != 4) {
        fprintf(stderr, "usage: width height frames");
        return 1;
    }

    int width = atoi(argv[1]);
    int height = atoi(argv[2]);
    int frames = atoi(argv[3]);

    fprintf(stderr, "width: %i\n", width);
    fprintf(stderr, "height: %i\n", height);
    fprintf(stderr, "frames: %i\n", frames);

    int x, y;

    CNSL_Display black = CNSL_AllocDisplay(width, height);
    for (x = 0; x < width; x++) {
        for (y = 0; y < height; y++) {
            CNSL_SetPixel(black, x, y, 0);
        }
    }

    CNSL_Display white = CNSL_AllocDisplay(width, height);
    for (x = 0; x < width; x++) {
        for (y = 0; y < height; y++) {
            CNSL_SetPixel(black, x, y, 0x00FFFFFF);
        }
    }

    int i;
    for (i = 0; i < frames; i++) {
        CNSL_Display dsp = i % 2 == 0 ? black : white;
        CNSL_SendDisplay(stdcon, dsp, 0, 0, 0, 0, width, height);
    }

    return 0;
}

