
#include <math.h>

#include "fonter.h"

int iceil(double x)
{
    return (int)ceil(x);
}


FNTR_Fonter FNTR_Create(const char* fontname)
{
    FNTR_Fonter fonter = (FNTR_Fonter)malloc(sizeof(FNTR_Fonter_));
    if (!fonter) {
        return NULL;
    }

    if (FT_Init_FreeType(&fonter->library) != 0) {
        free(fonter);
        return NULL;
    }

    FcInit();
    FcPattern* pattern = FcNameParse(fontname);
    FcConfigSubstitute(NULL, pattern, FcMatchPattern);
    FcDefaultSubstitute(pattern);
    FcPattern* match = FcFontMatch(NULL, pattern, NULL);

    FcValue file;
    FcValue psize;
    FcPatternGet(match, "file", 0, &file);
    FcPatternGet(match, "pixelsize", 0, &psize);

    int size = (int)ceil(psize.u.d);

    if (FT_New_Face(fonter->library, file.u.s, 0, &fonter->face) != 0) {
        return NULL;
    }

    FcPatternDestroy(pattern);
    FcPatternDestroy(match);

    if (FT_Set_Pixel_Sizes(fonter->face, size, size) != 0) {
        free(fonter);
        return NULL;
    }

    double scale = (double)size / fonter->face->units_per_EM;
    fonter->height = iceil(scale * fonter->face->height);
    fonter->ascender = iceil(scale * fonter->face->ascender);

    return fonter;
}

void FNTR_Free(FNTR_Fonter fonter)
{
    // TODO: what do we have to do to clean up the freetype stuff?
    free(fonter);
}


void FNTR_DrawString(FNTR_Fonter fonter, CNSL_Display display, CNSL_Color fg, CNSL_Color bg, int x, int y, const char* str)
{
    int fgr = CNSL_GetRed(fg);
    int fgg = CNSL_GetGreen(fg);
    int fgb = CNSL_GetBlue(fg);
    int bgr = CNSL_GetRed(bg);
    int bgg = CNSL_GetGreen(bg);
    int bgb = CNSL_GetBlue(bg);

    for ( ; *str; str++) {
        char c = *str;
        FT_UInt index = FT_Get_Char_Index(fonter->face, c);
        FT_Load_Glyph(fonter->face, index, FT_LOAD_RENDER);

        int bw = fonter->face->glyph->bitmap.width;
        int bh = fonter->face->glyph->bitmap.rows;
        int bl = fonter->face->glyph->bitmap_left;
        int bt = fonter->face->glyph->bitmap_top;

        int cw = fonter->face->glyph->metrics.horiAdvance >> 6;
        int ch = fonter->height;
        int ca = fonter->ascender;

        // blank the cell.
        int dx, dy;
        for (dy = 0; dy < ch; dy++) {
            for (dx = 0; dx < cw; dx++) {
                CNSL_SetPixel(display, x + dx, y - ch + dy, bg);
            }
        }

        // now draw the character.
        for (dy = 0; dy < bh; dy++) {
           for (dx = 0; dx < bw; dx++) {
               int index = dy*bw + dx;
               int level = fonter->face->glyph->bitmap.buffer[index];

               unsigned int red = 0xFF & ((fgr * level + bgr * (256-level))/256);
               unsigned int green = 0xFF & ((fgg * level + bgg * (256-level))/256);
               unsigned int blue = 0xFF & ((fgb * level + bgb * (256-level))/256);

               CNSL_Color c = CNSL_MakeColor(red, green, blue);
               CNSL_SetPixel(display, x + dx + bl, dy + ca - bt + (y-ch), c);
           }
        }

        x += cw;
    }
}

