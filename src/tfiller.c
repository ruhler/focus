
#include <assert.h>
#include <stdio.h>

#include "consoler.h"

void updatedisplay(CNSL_Client client, CNSL_Display display)
{
    int x, w, y, h;
    do {
        CNSL_RecvDisplay(client, &x, &y, &w, &h, CNSL_RDToDisplay, (void*)&display);
    } while (CNSL_PollDisplay(client));
}

int main(int argc, char* argv[]) 
{
    CNSL_Display display = CNSL_AllocDisplay(640, 480);
    CNSL_Event event;

    CNSL_Client filler = CNSL_LaunchClient(argv[1], argv+1);

    updatedisplay(filler, display);
    printf("%x\n", CNSL_GetPixel(display, 20, 30));
    assert(CNSL_GetPixel(display, 20, 30) == CNSL_MakeColor(0, 0, 0));

    event = CNSL_MakeKeypress(CNSLK_w);
    CNSL_SendEvent(filler, &event);
    updatedisplay(filler, display);
    printf("%x\n", CNSL_GetPixel(display, 20, 30));
    assert(CNSL_GetPixel(display, 20, 30) == CNSL_MakeColor(255, 255, 255));

    event = CNSL_MakeKeypress(CNSLK_q);

    CNSL_CloseClient(filler);
    return 0;
}

