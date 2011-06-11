
#include <assert.h>
#include <stdio.h>

#include "consoler.h"


// Call this with first command line argument the path to the pnger
// application to test, and second command line argument the path to a png
// file which is solid green and sized 64x128
int main(int argc, char* argv[])
{
    CNSL_Display display = CNSL_AllocDisplay(64, 128);

    CNSL_Client pnger = CNSL_LaunchClient(argv[1], argv+1);

    CNSL_RecvDisplay(pnger, display, NULL, NULL, NULL, NULL);
    assert(CNSL_GetPixel(display, 20, 30) == CNSL_MakeColor(0, 0xff, 0));

    CNSL_SendEvent(pnger, CNSL_MakeKeypress(CNSLK_q));
    CNSL_CloseClient(pnger);
    return 0;
}

