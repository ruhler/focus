
#include <pthread.h>

#include "ctermer.h"
#include "inputter.h"
#include "outputter.h"
#include "screen.h"

SCREEN_Screen scr;
SCREEN_Position oldcursor;
char* fromclientptr = NULL;

void drawcell(SCREEN_Position pos, const SCREEN_Cell* cell)
{
    int fgc = cell->cattrs.style.reverse ? cell->cattrs.bgcolor : cell->cattrs.fgcolor;
    int bgc = cell->cattrs.style.reverse ? cell->cattrs.fgcolor : cell->cattrs.bgcolor;
    int style = cell->cattrs.style.bold ? 1 : 0;

    ctermer_DrawCell(pos.column, pos.line, cell->character, style, fgc, bgc);
}

SCREEN_Cell curserify(SCREEN_Cell c)
{
    c.cattrs.fgcolor = SCREEN_COLOR_BLACK;
    c.cattrs.bgcolor = SCREEN_COLOR_WHITE;
    return c;
}

char getf()
{
    if (*fromclientptr == '\0') {
        // Update the screen now, then ask for more input.
        diff(&scr, drawcell);
        SCREEN_Cell cell = cellat(&scr, oldcursor);
        drawcell(oldcursor, &cell);

        oldcursor = cursor(&scr);
        cell = curserify(cellat(&scr, oldcursor));
        drawcell(oldcursor, &cell);
        ctermer_ShowDisplay();

        fromclientptr = ctermer_FromTermClient();
    }

    char c = *fromclientptr;
    fromclientptr++;
    return c;
}

CNSL_Event getevent()
{
    ctermer_EventGet();

    CNSL_Event e;
    e.type = ctermer_EventType();
    e.value = ctermer_EventValue();
    return e;
}
    

void* runoutputter(void* ud)
{
    fromclientptr = ctermer_FromTermClient();
    oldcursor = mkpos(0, 0);
    outputter(&scr, '\0', getf);
    ctermer_Quit();
}
    

int main()
{
    ctermer_Init();

    scr = screen(80, 24);

    pthread_t othread;
    pthread_create(&othread, NULL, &runoutputter, NULL);
    inputter(getevent, ctermer_ToTermClient);
    ctermer_DeInit();
    return 0;
}

