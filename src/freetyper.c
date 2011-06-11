
#include <stdio.h>
#include <png.h>

#include <ft2build.h>
#include FT_FREETYPE_H

#include "consoler.h"

// freetyper
// Is really just an application for me to experiment with freetype rendering
// of fonts.

#define WIDTH 640
#define HEIGHT 480

int main(int argc, char* argv[])
{
    char* font = "/pkg/dejavu-fonts-ttf-2.32/dejavu-fonts-ttf-2.32/ttf/DejaVuSansMono-Bold.ttf";
    int size = 32;
    char character = 'A';

    CNSL_Init();
    CNSL_Display display = CNSL_AllocDisplay(WIDTH, HEIGHT);

    FT_Library lib;
    FT_Face face;

    fprintf(stderr, "init: %i\n", FT_Init_FreeType(&lib));
    fprintf(stderr, "face: %i\n", FT_New_Face(lib, font, 0, &face));
    fprintf(stderr, "sizes: %i\n", FT_Set_Pixel_Sizes(face, size, size));
    fprintf(stderr, "char: %i\n", FT_Load_Char(face, character, FT_LOAD_RENDER));

    fprintf(stderr, "pixel mode: %i\n", face->glyph->bitmap.pixel_mode);
    fprintf(stderr, "num grays: %i\n", face->glyph->bitmap.num_grays);

    int x, y;
    int width = face->glyph->bitmap.width;
    int height = face->glyph->bitmap.rows;
    fprintf(stderr, "width: %i\n", width);
    fprintf(stderr, "height: %i\n", height);
    fprintf(stderr, "bitmap left: %i\n", face->glyph->bitmap_left);
    fprintf(stderr, "bitmap top: %i\n", face->glyph->bitmap_top);

    for (x = 0; x < width; x++) {
        for (y = 0; y <  height; y++) {
            int index = y * width + x;
            int level = face->glyph->bitmap.buffer[index];
            CNSL_Color c = CNSL_MakeColor(level, level, 0);
            CNSL_SetPixel(display, 10 + x, 10 + y, c);
        }
    }

    CNSL_SendDisplay(stdcon, display, 0, 0, 0, 0, WIDTH, HEIGHT);

    CNSL_Event event;

    int done = 0;
    while (!done) {
        CNSL_RecvEvent(stdcon, &event);
        int sym;

        if (CNSL_IsKeypress(event, &sym)) {
            switch (sym) {
                case CNSLK_q:
                    done = 1;
                    break;
            }
        }
    }

    CNSL_Quit();
}

