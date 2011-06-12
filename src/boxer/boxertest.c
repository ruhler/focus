

#include <assert.h>
#include <stdio.h>

#include "consoler.h"

// Call this with first command line argument the path to the boxer
// application to test.
int main(int argc, char* argv[]) 
{
    CNSL_Display display = CNSL_AllocDisplay(640, 480);
    CNSL_Event event;
    CNSL_Client boxer = CNSL_LaunchClient(argv[1], argv+1);
    CNSL_Color white = CNSL_MakeColor(0xff, 0xff, 0xff);
    CNSL_Color red = CNSL_MakeColor(0xff, 0x00, 0x00);
    CNSL_Color black = CNSL_MakeColor(0x00, 0x00, 0x00);
    CNSL_Color color;

    CNSL_RecvDisplay(boxer, display, NULL, NULL, NULL, NULL);
    color = CNSL_GetPixel(display, 105, 105);
    fprintf(stderr, "%08x\n", color);
    assert(color == white);

    CNSL_SendEvent(boxer, CNSL_MakeKeypress(CNSLK_j));
    CNSL_RecvDisplay(boxer, display, NULL, NULL, NULL, NULL);
    color = CNSL_GetPixel(display, 105, 105);
    assert(color == red);
    color = CNSL_GetPixel(display, 105, 115);
    assert(color == white);
    color = CNSL_GetPixel(display, 115, 105);
    assert(color == black);

    event = CNSL_MakeKeypress(CNSLK_q);
    CNSL_SendEvent(boxer, event);

    CNSL_CloseClient(boxer);
    return 0;
}

