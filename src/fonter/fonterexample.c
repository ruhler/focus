
#include <stdio.h>

#include "consoler.h"
#include "fonter.h"

int main()
{
    const char* fontname = "Monospace-25:Bold";
    const char* string = "Hello there! goodbye!";

    int width = 640;
    int height = 480;
    CNSL_GetGeometry(&width, &height);

    CNSL_Display display = CNSL_AllocDisplay(width, height);

    CNSL_Event event;
    CNSL_Keysym sym;
    bool done = false;

    CNSL_Color fg = CNSL_MakeColor(0xff, 0xff, 0xff);
    CNSL_Color bg = CNSL_MakeColor(0x00, 0x00, 0xff);

    int x = 30;
    int y = 40;

    FNTR_Fonter fonter = FNTR_Create(fontname);
    FNTR_DrawString(fonter, display, fg, bg, x, y, string);

    while (!done) {
        CNSL_SendDisplay(stdcon, display, 0, 0, 0, 0, width, height);

        event = CNSL_RecvEvent(stdcon);
        if (CNSL_IsQuit(event) || (CNSL_IsKeypress(event, &sym) && sym == CNSLK_q)) {
            done = true;
            break;
        }
    }

    FNTR_Free(fonter);
    return 0;
}

