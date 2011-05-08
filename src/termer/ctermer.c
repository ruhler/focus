
#include <pty.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>

#include <ft2build.h>
#include FT_FREETYPE_H

#include "consoler.h"
#include "ctermer.h"

#define FONT "/usr/share/fonts/truetype/ttf-dejavu/DejaVuSansMono-Bold.ttf"
//#define FONT "/pkg/dejavu-fonts-ttf-2.32/dejavu-fonts-ttf-2.32/ttf/DejaVuSansMono-Bold.ttf"

#define WIDTH 640
#define HEIGHT 480
#define CHAR_WIDTH 32
#define CHAR_HEIGHT 32


typedef struct {
    // The most recently gotten event.
    CNSL_Event event;
    CNSL_Display display;

    FT_Library library;
    FT_Face face;
    
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
        execl("/bin/sh", "/bin/sh", NULL);
        perror("execl");
        exit(1);
    }
    return 0;
}


int ctermer_Init()
{
    const int size = 32;

    if (forkterminalclient() != 0) {
        fprintf(stderr, "error forking terminal client\n");
        return 1;
    }

    if (FT_Init_FreeType(&gstate.library) != 0) {
        fprintf(stderr, "error initializing freetype\n");
        return 1;
    }

    if (FT_New_Face(gstate.library, FONT, 0, &gstate.face) != 0) {
        fprintf(stderr, "error loading font\n");
        return 1;
    }

    if (FT_Set_Pixel_Sizes(gstate.face, size, size) != 0) {
        fprintf(stderr, "error setting font size\n");
        return 1;
    }

    CNSL_Init();
    gstate.display = CNSL_AllocDisplay(WIDTH, HEIGHT);

    return 0;
}

void ctermer_DeInit()
{
    CNSL_Quit();
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

char ctermer_FromTermClient()
{
    char c = '\0';
    read(gstate.tcfd, &c, 1);
    return c;
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

void ctermer_DrawCell(int col, int row, char c, int style, int fgcolor, int bgcolor)
{
    //fprintf(stderr, "dc %c(%02X) @ %ix%i\n", c, c, col, row);

    FT_Load_Char(gstate.face, c, FT_LOAD_RENDER);

    //fprintf(stderr, "advance.x: %li\n", gstate.face->glyph->advance.x);
    //fprintf(stderr, "advance.y: %li\n", gstate.face->glyph->advance.y);
    //fprintf(stderr, "metrics.height: %li\n", gstate.face->size->metrics.height);
    //fprintf(stderr, "max_advance: %li\n", gstate.face->size->metrics.max_advance);
    //fprintf(stderr, "units_per_EM: %i\n", gstate.face->units_per_EM);
    //fprintf(stderr, "height: %i\n", gstate.face->height);
    //fprintf(stderr, "max_advance_width: %i\n", gstate.face->max_advance_width);
    //fprintf(stderr, "bbox xmin: %li\n", gstate.face->bbox.xMin);
    //fprintf(stderr, "bbox xman: %li\n", gstate.face->bbox.xMax);
    //fprintf(stderr, "bbox ymin: %li\n", gstate.face->bbox.yMin);
    //fprintf(stderr, "bbox yman: %li\n", gstate.face->bbox.yMax);

    int xdst = col * CHAR_WIDTH;
    int ydst = row * CHAR_HEIGHT;

    int w = gstate.face->glyph->bitmap.width;
    int h = gstate.face->glyph->bitmap.rows;
    int l = gstate.face->glyph->bitmap_left;
    int t = gstate.face->glyph->bitmap_top;

    int x, y;
    for (x = 0; x < w; x++) {
        for (y = 0; y < h; y++) {
            int index = y * w + x;
            int level = gstate.face->glyph->bitmap.buffer[index];
            int red = 0xFF & ((redof(fgcolor, style) * level + redof(bgcolor, style) * (256-level))/256);
            int green = 0xFF & ((greenof(fgcolor, style) * level + greenof(bgcolor, style) * (256-level))/256);
            int blue = 0xFF & ((blueof(fgcolor, style) * level + blueof(bgcolor, style) * (256-level))/256);
            CNSL_Color c = CNSL_MakeColor(red, green, blue);

            // TODO: if I do things right, I shouldn't have to do a check
            // here!
            if (ydst + CHAR_HEIGHT - t + y >= 0) {
                CNSL_SetPixel(gstate.display, xdst + l + x, ydst + CHAR_HEIGHT - t + y, c);
            }
        }
    }
}

void ctermer_ShowDisplay()
{
    CNSL_SendDisplay(stdcon, gstate.display, 0, 0, 0, 0, WIDTH, HEIGHT);
}

