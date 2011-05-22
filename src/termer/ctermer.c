
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
#include "ctermer.h"

#define WIDTH 640
#define HEIGHT 480

typedef struct {
    // The most recently gotten event.
    CNSL_Event event;
    CNSL_Display display;

    FT_Library library;
    FT_Face face;

    // Character metrics.
    int cell_width;
    int cell_height;
    int char_ascender;
         
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
        // does not return (I hope).
        execl("/bin/sh", "/bin/sh", "src/termer/bench.sh", NULL);
        perror("execl");
        exit(1);
    }
    return 0;
}


int ctermer_Init()
{
    const char* font = "Monospace:Bold";
    const int size = 12;

    if (forkterminalclient() != 0) {
        fprintf(stderr, "error forking terminal client\n");
        return 1;
    }

    if (FT_Init_FreeType(&gstate.library) != 0) {
        fprintf(stderr, "error initializing freetype\n");
        return 1;
    }

    // Find a font file using fontconfig
    FcInit();
    FcPattern* pattern = FcNameParse(font);
    FcConfigSubstitute(NULL, pattern, FcMatchPattern);
    FcDefaultSubstitute(pattern);
    FcPattern* match = FcFontMatch(NULL, pattern, NULL);

    FcValue file;
    FcPatternGet(match, "file", 0, &file);

    if (FT_New_Face(gstate.library, file.u.s, 0, &gstate.face) != 0) {
        fprintf(stderr, "error loading font\n");
        return 1;
    }

    FcPatternDestroy(pattern);
    FcPatternDestroy(match);

    if (FT_Set_Pixel_Sizes(gstate.face, size, size) != 0) {
        fprintf(stderr, "error setting font size\n");
        return 1;
    }

    double em = (double)gstate.face->units_per_EM;
    double w = (double)gstate.face->max_advance_width;
    double h = (double)gstate.face->height;
    double a = (double)gstate.face->ascender;
    double p = (double)size;
    gstate.cell_width = (int)ceil(w*p/em);
    gstate.cell_height = (int)ceil(h*p/em);
    gstate.char_ascender = (int)ceil(a*p/em);


    CNSL_Init();
    gstate.display = CNSL_AllocDisplay(WIDTH, HEIGHT);
    gstate.mincol = -1;
    gstate.maxcol = -1;
    gstate.minrow = -1;
    gstate.maxrow = -1;

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

    FT_Load_Char(gstate.face, c, FT_LOAD_RENDER);

    int xdst = col * gstate.cell_width;
    int ydst = row * gstate.cell_height;

    int w = gstate.face->glyph->bitmap.width;
    int h = gstate.face->glyph->bitmap.rows;
    int l = gstate.face->glyph->bitmap_left;
    int t = gstate.face->glyph->bitmap_top;

    int x, y;

    // blank the cell first
    int bgc = (redof(bgcolor, style) << 16) | (greenof(bgcolor, style) << 8) | blueof(bgcolor, style);
    for (x = 0; x < gstate.cell_width; x++) {
        for (y = 0; y < gstate.cell_height; y++) {
            CNSL_SetPixel(gstate.display, xdst + x, ydst + y, bgc);
        }
    }

    // Now draw the character.
    for (x = 0; x < w; x++) {
        for (y = 0; y < h; y++) {
            int index = y * w + x;
            int level = gstate.face->glyph->bitmap.buffer[index];

            unsigned int red = 0xFF & ((redof(fgcolor, style) * level + redof(bgcolor, style) * (256-level))/256);
            unsigned int green = 0xFF & ((greenof(fgcolor, style) * level + greenof(bgcolor, style) * (256-level))/256);
            unsigned int blue = 0xFF & ((blueof(fgcolor, style) * level + blueof(bgcolor, style) * (256-level))/256);
            CNSL_Color c = CNSL_MakeColor(red, green, blue);

            int px = xdst + l + x;
            int py = ydst + gstate.char_ascender-t+y;
            CNSL_SetPixel(gstate.display, px, py, c);
        }
    }
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

        if (x+w > WIDTH) {
            w = WIDTH - x;
        }

        if (y+h > HEIGHT) {
            h = HEIGHT - y;
        }
    }

    CNSL_SendDisplay(stdcon, gstate.display, x, y, x, y, w, h);

    gstate.mincol = -1;
    gstate.maxcol = -1;
    gstate.minrow = -1;
    gstate.maxrow = -1;
}


