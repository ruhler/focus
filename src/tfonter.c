
#include <stdio.h>

#include "consoler.h"
#include "fonter.h"

int main()
{
    CNSL_Init();

    const char* fontname = "Monospace-25:Bold";
    const char* string = "Hello there! goodbye!";

    int width = 640;
    int height = 480;
    CNSL_GetGeometry(&width, &height);

    CNSL_Display display = CNSL_AllocDisplay(width, height);

    CNSL_Event event;
    int sym;
    int done = 0;

    CNSL_Color fg = CNSL_MakeColor(0xff, 0xff, 0xff);
    CNSL_Color bg = CNSL_MakeColor(0x00, 0x00, 0xff);

    int x = 30;
    int y = 40;

    FNTR_Fonter fonter = FNTR_Create(fontname);
    FNTR_DrawString(fonter, display, fg, bg, x, y, string);

    while (!done) {
        int x, y;
        CNSL_SendDisplay(stdcon, display, 0, 0, 0, 0, width, height);

        if (CNSL_RecvEvent(stdcon, &event) == 0) {
            done = 1;
            break;
        }

        if (CNSL_IsKeypress(event, &sym) && sym == CNSLK_q) {
            done = 1;
            break;
        }
    }

    FNTR_Free(fonter);
    CNSL_Quit();
    return 0;
}

