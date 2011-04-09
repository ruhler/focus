
#include <stdio.h>

#include "consoler.h"

// Application which fills the screen with solid colors.
// The colors changed based on key presses.
//
// g: Green     0 1 0
// b: Blue      0 0 1
// r: Red       1 0 0
// c: Cyan      0 1 1
// y: Yellow    1 0 1
// p: Purple    1 1 0
// w: White     1 1 1
// n: Nothing   0 0 0
// q: Quit

#define WIDTH 640
#define HEIGHT 480

#define BLACK 0
#define RED 1
#define GREEN 2
#define BLUE 3
#define CYAN 4
#define YELLOW 5
#define PURPLE 6
#define WHITE 7

int main()
{
    CNSL_Init();

    CNSL_Color colors[8];
    colors[BLACK] = CNSL_MakeColor(0, 0, 0);
    colors[RED] = CNSL_MakeColor(255, 0, 0);
    colors[GREEN] = CNSL_MakeColor(0, 255, 0);
    colors[BLUE] = CNSL_MakeColor(0, 0, 255);
    colors[CYAN] = CNSL_MakeColor(0, 255, 255);
    colors[YELLOW] = CNSL_MakeColor(255, 0, 255);
    colors[PURPLE] = CNSL_MakeColor(255, 255, 0);
    colors[WHITE] = CNSL_MakeColor(255, 255, 255);

    CNSL_Display display = CNSL_AllocDisplay(WIDTH, HEIGHT);
    CNSL_Event event;
    int sym;
    int done = 0;
    int color = BLACK;
    while (!done) {
        int x, y;
        fprintf(stderr, "filler: filling with color %x\n", colors[color]);
        for (x = 0; x < WIDTH; x++) {
            for (y = 0; y < HEIGHT; y++) {
                CNSL_SetPixel(display, x, y, colors[color]);
            }
        }
        CNSL_SendDisplay(stdcon, display, 0, 0, 0, 0, WIDTH, HEIGHT);

        if (CNSL_RecvEvent(stdcon, &event) == 0) {
            done = 1;
            break;
        }

        if (CNSL_IsKeypress(&event, &sym)) {
            fprintf(stderr, "filler: got keysym %c\n", sym);
            switch (sym) {
                case 'q': done = 1; break;
                case 'r': color = RED; break;
                case 'g': color = GREEN; break;
                case 'b': color = BLUE; break;
                case 'c': color = CYAN; break;
                case 'y': color = YELLOW; break;
                case 'p': color = PURPLE; break;
                case 'w': color = WHITE; break;
                case 'n': color = BLACK; break;
            }
        }
    }

    CNSL_Quit();
    return 0;
}


