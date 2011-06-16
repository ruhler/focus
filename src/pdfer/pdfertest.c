
#include <assert.h>
#include <stdio.h>

#include "consoler.h"

// Call this with first command line argument the path to the pdfer
// application to test, and second command line argument the path to the
// colors.pdf file 
// 
// The colors.pdf file has 8 pages, each 8.5" by 11" with a solid color:
//  black, blue, green, cyan, red, purple, yellow, white
int main(int argc, char* argv[])
{
    CNSL_Display display = CNSL_AllocDisplay(640, 480);
    CNSL_Client pdfer = CNSL_LaunchClient(argv[1], argv+1);
    CNSL_Color color;

    CNSL_RecvDisplay(pdfer, display, NULL, NULL, NULL, NULL);

    CNSL_SendEvent(pdfer, CNSL_MakeKeypress(CNSLK_w));
    CNSL_SendEvent(pdfer, CNSL_MakeKeyrelease(CNSLK_w));
    CNSL_RecvDisplay(pdfer, display, NULL, NULL, NULL, NULL);
    color = 0xFFFFFF & CNSL_GetPixel(display, 20, 30);
    fprintf(stderr, "%08x\n", color);
    assert(color == CNSL_MakeColor(0, 0, 0));

    CNSL_SendEvent(pdfer, CNSL_MakeKeypress(CNSLK_n));
    CNSL_SendEvent(pdfer, CNSL_MakeKeyrelease(CNSLK_n));
    CNSL_RecvDisplay(pdfer, display, NULL, NULL, NULL, NULL);
    color = 0xFFFFFF & CNSL_GetPixel(display, 20, 30);
    fprintf(stderr, "%08x\n", color);
    assert(color == CNSL_MakeColor(0, 0, 0xff));

    CNSL_SendEvent(pdfer, CNSL_MakeKeypress(CNSLK_n));
    CNSL_SendEvent(pdfer, CNSL_MakeKeyrelease(CNSLK_n));
    CNSL_RecvDisplay(pdfer, display, NULL, NULL, NULL, NULL);
    color = 0xFFFFFF & CNSL_GetPixel(display, 20, 30);
    fprintf(stderr, "%08x\n", color);
    assert(color == CNSL_MakeColor(0, 0xff, 0));

    CNSL_SendEvent(pdfer, CNSL_MakeKeypress(CNSLK_q));
    CNSL_CloseClient(pdfer);
    return 0;
}

