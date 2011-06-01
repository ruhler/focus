
#include <stdio.h>

#include "consoler.h"
#include "kmapper.h"

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

void tored(int* c) { *c = RED; }
void togreen(int* c) { *c = GREEN; }
void toblue(int* c) { *c = BLUE; }
void tocyan(int* c) { *c = CYAN; }
void toyellow(int* c) { *c = YELLOW; }
void topurple(int* c) { *c = PURPLE; }
void towhite(int* c) { *c = WHITE; }
void toblack(int* c) { *c = BLACK; }
void quit(int* done) { *done = 1; }

int main()
{
    CNSL_Init();

    CNSL_Color colors[8];
    colors[BLACK] = CNSL_MakeColor(0, 0, 0);
    colors[RED] = CNSL_MakeColor(255, 0, 0);
    colors[GREEN] = CNSL_MakeColor(0, 255, 0);
    colors[BLUE] = CNSL_MakeColor(0, 0, 255);
    colors[CYAN] = CNSL_MakeColor(0, 255, 255);
    colors[YELLOW] = CNSL_MakeColor(255, 255, 0);
    colors[PURPLE] = CNSL_MakeColor(255, 0, 255);
    colors[WHITE] = CNSL_MakeColor(255, 255, 255);


    CNSL_Display display = CNSL_AllocDisplay(WIDTH, HEIGHT);
    CNSL_Event event;
    int sym;
    int done = 0;
    int color = BLACK;

    KMPR_KMapper kmapper = KMPR_Create();
    KMPR_RegisterAction(kmapper, CNSLK_q, (KMPR_Action)&quit, &done);
    KMPR_RegisterAction(kmapper, CNSLK_r, (KMPR_Action)&tored, &color);
    KMPR_RegisterAction(kmapper, CNSLK_g, (KMPR_Action)&togreen, &color);
    KMPR_RegisterAction(kmapper, CNSLK_b, (KMPR_Action)&toblue, &color);
    KMPR_RegisterAction(kmapper, CNSLK_c, (KMPR_Action)&tocyan, &color);
    KMPR_RegisterAction(kmapper, CNSLK_y, (KMPR_Action)&toyellow, &color);
    KMPR_RegisterAction(kmapper, CNSLK_p, (KMPR_Action)&topurple, &color);
    KMPR_RegisterAction(kmapper, CNSLK_w, (KMPR_Action)&towhite, &color);
    KMPR_RegisterAction(kmapper, CNSLK_n, (KMPR_Action)&toblack, &color);

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

        KMPR_NextEvent(kmapper, &event);
    }

    KMPR_Free(kmapper);
    CNSL_Quit();
    return 0;
}


