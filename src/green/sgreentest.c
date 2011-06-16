
#include <assert.h>
#include <stdio.h>

#include "consoler.h"

// Call this with
//  first arg: path to sgreen
//  second arg: path to filler
int main(int argc, char* argv[])
{
    const char* sgreenpath = argv[1];
    const char* fillerpath = argv[2];
    char* noargs[] = {NULL};

    CNSL_Display display = CNSL_AllocDisplay(64, 48);
    CNSL_Event event;

    setenv("CNSLSHELL", fillerpath, 1);
    CNSL_Client sgreen = CNSL_LaunchClient(sgreenpath, noargs);

    // Verify we can interact with filler as expected.
    // first recv display is from clearing the client display
    // second recv display is from filler starting up
    CNSL_RecvDisplay(sgreen, display, NULL, NULL, NULL, NULL);
    CNSL_RecvDisplay(sgreen, display, NULL, NULL, NULL, NULL);
    printf("%x\n", CNSL_GetPixel(display, 20, 30));
    assert(CNSL_GetPixel(display, 20, 30) == CNSL_MakeColor(0, 0, 0));

    event = CNSL_MakeKeypress(CNSLK_w);
    CNSL_SendEvent(sgreen, event);
    CNSL_RecvDisplay(sgreen, display, NULL, NULL, NULL, NULL);
    printf("%x\n", CNSL_GetPixel(display, 20, 30));
    assert(CNSL_GetPixel(display, 20, 30) == CNSL_MakeColor(255, 255, 255));

    event = CNSL_MakeKeypress(CNSLK_r);
    CNSL_SendEvent(sgreen, event);
    CNSL_RecvDisplay(sgreen, display, NULL, NULL, NULL, NULL);
    printf("%x\n", CNSL_GetPixel(display, 20, 30));
    assert(CNSL_GetPixel(display, 20, 30) == CNSL_MakeColor(255, 0, 0));

    event = CNSL_MakeKeypress(CNSLK_q);
    CNSL_SendEvent(sgreen, event);
    return 0;
}

