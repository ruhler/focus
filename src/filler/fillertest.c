
#include <assert.h>
#include <stdio.h>

#include "consoler.h"

// Call this with first command line argument the path to the filler
// application to test.
int main(int argc, char* argv[]) 
{
    CNSL_Display display = CNSL_AllocDisplay(640, 480);
    CNSL_Event event;

    CNSL_Client filler = CNSL_LaunchClient(argv[1], argv+1);

    CNSL_RecvDisplay(filler, display, NULL, NULL, NULL, NULL);
    printf("%x\n", CNSL_GetPixel(display, 20, 30));
    assert(CNSL_GetPixel(display, 20, 30) == CNSL_MakeColor(0, 0, 0));

    event = CNSL_MakeKeypress(CNSLK_w);
    CNSL_SendEvent(filler, event);
    CNSL_RecvDisplay(filler, display, NULL, NULL, NULL, NULL);
    printf("%x\n", CNSL_GetPixel(display, 20, 30));
    assert(CNSL_GetPixel(display, 20, 30) == CNSL_MakeColor(255, 255, 255));

    event = CNSL_MakeKeypress(CNSLK_r);
    CNSL_SendEvent(filler, event);
    CNSL_RecvDisplay(filler, display, NULL, NULL, NULL, NULL);
    printf("%x\n", CNSL_GetPixel(display, 20, 30));
    assert(CNSL_GetPixel(display, 20, 30) == CNSL_MakeColor(255, 0, 0));

    event = CNSL_MakeKeypress(CNSLK_q);
    CNSL_SendEvent(filler, event);

    CNSL_CloseClient(filler);
    return 0;
}

