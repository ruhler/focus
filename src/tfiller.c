
#include <assert.h>

#include "consoler.h"

void todisplay(void* vb, int x, int y, CNSL_Color c)
{
    CNSL_Display b = (CNSL_Display)vb;
    if (x >= 0 && x < b->width && y >= 0 && y < b->height) {
        CNSL_SetPixel(b, x, y, c);
    }
}

void updatedisplay(CNSL_Client client, CNSL_Display display)
{
    int x, w, y, h;
    do {
        CNSL_RecvDisplay(client, &x, &y, &w, &h, todisplay, (void*)display);
    } while (CNSL_PollDisplay(client));
}

int main(int argc, char* argv[]) 
{
    CNSL_Init();

    CNSL_Display display = CNSL_AllocDisplay(640, 480);
    CNSL_Event event;

    CNSL_Client filler = CNSL_LaunchClient(argv[1], argv+1);

    updatedisplay(filler, display);
    printf("%x\n", CNSL_GetPixel(display, 20, 30));
    assert(CNSL_GetPixel(display, 20, 30) == CNSL_MakeColor(0, 0, 0));

    event.type = CNSLE_KEYPRESS;
    event.value = CNSLK_w;
    CNSL_SendEvent(filler, &event);
    updatedisplay(filler, display);
    printf("%x\n", CNSL_GetPixel(display, 20, 30));
    assert(CNSL_GetPixel(display, 20, 30) == CNSL_MakeColor(255, 255, 255));

    event.value = CNSLK_q;

    CNSL_CloseClient(filler);
    CNSL_Quit();
    return 0;
}

