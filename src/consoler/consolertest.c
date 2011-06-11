
#include <assert.h>
#include <stdio.h>

#include "consoler.h"

int main(int argc, char* argv[])
{
    CNSL_Keysym sym;
    CNSL_Event event;
   
    event = CNSL_MakeKeypress(CNSLK_a);
    assert(CNSL_IsKeypress(event, &sym));
    assert(!CNSL_IsKeyrelease(event, NULL));
    assert(!CNSL_IsQuit(event));
    assert(sym == CNSLK_a);

    event = CNSL_MakeKeyrelease(CNSLK_b);
    assert(!CNSL_IsKeypress(event, NULL));
    assert(CNSL_IsKeyrelease(event, &sym));
    assert(!CNSL_IsQuit(event));
    assert(sym == CNSLK_b);

    event = CNSL_MakeQuit();
    assert(!CNSL_IsKeypress(event, NULL));
    assert(!CNSL_IsKeyrelease(event, NULL));
    assert(CNSL_IsQuit(event));

    CNSL_Color color = CNSL_MakeColor(0x12, 0x34, 0x56);
    assert(CNSL_GetRed8(color) == 0x12);
    assert(CNSL_GetGreen8(color) == 0x34);
    assert(CNSL_GetBlue8(color) == 0x56);
    assert(CNSL_GetRGB8(color) == 0x123456);

    printf("all tests passed\n");
    return 0;
}

