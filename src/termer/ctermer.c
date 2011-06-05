
#include <assert.h>
#include <math.h>
#include <pty.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>

#include <fontconfig.h>

#include <ft2build.h>
#include FT_FREETYPE_H

#include "consoler.h"
#include "fonter.h"
#include "ctermer.h"

typedef struct {
    // The most recently gotten event.
    CNSL_Event event;
    CNSL_Display display;

    FNTR_Fonter fonter;

    // Window size
    int width;
    int height;

    // Character metrics.
    int cell_width;
    int cell_height;
         
    // bounds of cells which have changed and need redisplay
    // -1 means they have not been set yet.
    int maxcol;
    int mincol;
    int maxrow;
    int minrow;
    
    // terminal client file descriptor.
    int tcfd;
} ctermer_state;

ctermer_state gstate;

int forkterminalclient()
{
    setenv("TERM", "termer", 1);

    pid_t pid = forkpty(&gstate.tcfd, NULL, NULL, NULL);
    if (pid < 0) {
        perror("forkpty");
        return 1;
    }

    if (pid == 0) {
        const char* shell = getenv("SHELL");
        if (!shell) {
            shell = "bin/sh";
        }
        
        execl(shell, shell, NULL);
        perror("execl");
        exit(1);
    }
    return 0;
}


int ctermer_Init(int* cols, int* lines)
{
    const char* font = getenv("TERMERFONT");
    if (!font) {
        font = "Monospace";
    }

    gstate.fonter = FNTR_Create(font);
    gstate.cell_width = FNTR_MaxWidth(gstate.fonter);
    gstate.cell_height = FNTR_MaxHeight(gstate.fonter);

    gstate.width = 640;
    gstate.height = 480;
    CNSL_GetGeometry(&gstate.width, &gstate.height);

    CNSL_Init();
    gstate.display = CNSL_AllocDisplay(gstate.width, gstate.height);
    gstate.mincol = -1;
    gstate.maxcol = -1;
    gstate.minrow = -1;
    gstate.maxrow = -1;

    *cols = gstate.width / gstate.cell_width;
    *lines = gstate.height / gstate.cell_height;
    char colsstr[10] = {0};
    char linesstr[10] = {0};
    snprintf(colsstr, 10, "%i", *cols);
    snprintf(linesstr, 10, "%i", *lines);
    setenv("COLUMNS", colsstr, 1);
    setenv("LINES", linesstr, 1);

    if (forkterminalclient() != 0) {
        fprintf(stderr, "error forking terminal client\n");
        return 1;
    }

    return 0;
}

void ctermer_DeInit()
{
    CNSL_Quit();
}

void ctermer_Quit()
{
    // TODO: I'm not sure how to signal to ctermer_EventGet.
    assert(0 && "todo: ctermer_Quit");
}

void ctermer_EventGet()
{
    CNSL_RecvEvent(stdcon, &gstate.event);
}

int ctermer_EventType()
{
    return gstate.event.type;
}

int ctermer_EventValue()
{
    return gstate.event.value;
}

void ctermer_ToTermClient(char c)
{
    write(gstate.tcfd, &c, 1);
}

char fromtermclientbuf[BUFSIZ+1];

char* ctermer_FromTermClient()
{
    int red = read(gstate.tcfd, fromtermclientbuf, BUFSIZ);
    if (red < 0) {
        perror("read");
        red = 0;
    }
    fromtermclientbuf[red] = '\0';
    return fromtermclientbuf;
}

int redof(int color, int style)
{
    int full = style == CTERMER_STYLE_BOLD ? 0xFF : 0xAF;
    return color & CTERMER_COLOR_RED ? full : 0;
}

int greenof(int color, int style)
{
    int full = style == CTERMER_STYLE_BOLD ? 0xFF : 0xAF;
    return color & CTERMER_COLOR_GREEN ? full : 0;
}
int blueof(int color, int style)
{
    int full = style == CTERMER_STYLE_BOLD ? 0xFF : 0xAF;
    return color & CTERMER_COLOR_BLUE ? full : 0;
}

double fromfixed(signed long x)
{
    double d = x;
    d /= 65536.0;
    return d;
}

void ctermer_DrawCell(int col, int row, wchar_t c, int style, int fgcolor, int bgcolor)
{
    if (gstate.mincol == -1 || gstate.mincol > col) {
        gstate.mincol = col;
    }
    if (gstate.maxcol == -1 || gstate.maxcol < col) {
        gstate.maxcol = col;
    }
    if (gstate.minrow == -1 || gstate.minrow > row) {
        gstate.minrow = row;
    }
    if (gstate.maxrow == -1 || gstate.maxrow < row) {
        gstate.maxrow = row;
    }

    CNSL_Color fg = CNSL_MakeColor(redof(fgcolor, style), greenof(fgcolor, style), blueof(fgcolor, style));
    CNSL_Color bg = CNSL_MakeColor(redof(bgcolor, style), greenof(bgcolor, style), blueof(bgcolor, style));
    int x = col * gstate.cell_width;
    int y = row * gstate.cell_height;

    FNTR_LoadGlyph(gstate.fonter, c);
    FNTR_DrawGlyph(gstate.fonter, gstate.display, fg, bg, x, y);
}

void ctermer_ShowDisplay()
{
    int x = 0;
    int y = 0;
    int w = 0;
    int h = 0;

    if (gstate.mincol != -1) {
        assert(gstate.maxcol != -1);
        assert(gstate.minrow != -1);
        assert(gstate.maxrow != -1);

        x = gstate.cell_width * gstate.mincol;
        y = gstate.cell_height * gstate.minrow;
        w = gstate.cell_width * (gstate.maxcol - gstate.mincol + 1);
        h = gstate.cell_height * (gstate.maxrow - gstate.minrow + 1);

        if (x+w > gstate.width) {
            w = gstate.width - x;
        }

        if (y+h > gstate.height) {
            h = gstate.height - y;
        }
    }

    CNSL_SendDisplay(stdcon, gstate.display, x, y, x, y, w, h);

    gstate.mincol = -1;
    gstate.maxcol = -1;
    gstate.minrow = -1;
    gstate.maxrow = -1;
}

