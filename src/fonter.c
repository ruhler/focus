
#include <math.h>

#include "fonter.h"

int from26_6(int x)
{
    return x >> 6;
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

    fonter->width = from26_6(fonter->face->size->metrics.max_advance);
    fonter->height = from26_6(fonter->face->size->metrics.height);
    fonter->ascender = from26_6(fonter->face->size->metrics.ascender);

    return fonter;
}

void FNTR_Free(FNTR_Fonter fonter)
{
    // TODO: what do we have to do to clean up the freetype stuff?
    free(fonter);
}

int FNTR_MaxWidth(FNTR_Fonter fonter)
{
    return fonter->width;
}

int FNTR_MaxHeight(FNTR_Fonter fonter)
{
    return fonter->height;
}

void FNTR_LoadGlyph(FNTR_Fonter fonter, wchar_t c)
{
    FT_UInt index = FT_Get_Char_Index(fonter->face, c);
    FT_Load_Glyph(fonter->face, index, FT_LOAD_RENDER);
}

int FNTR_GlyphWidth(FNTR_Fonter fonter)
{
    return from26_6(fonter->face->glyph->metrics.horiAdvance);
}

int FNTR_GlyphHeight(FNTR_Fonter fonter)
{
    return fonter->height;
}

int FNTR_GlyphLevel(FNTR_Fonter fonter, int x, int y)
{
    int left = fonter->face->glyph->bitmap_left;
    int width = fonter->face->glyph->bitmap.width;
    int top = fonter->ascender - fonter->face->glyph->bitmap_top;
    int height = fonter->face->glyph->bitmap.rows;
    if (x < left || x >= left + width || y < top || y >= top + height) {
        return 0;
    }

    int index = (y - top) * width + (x - left);
    return fonter->face->glyph->bitmap.buffer[index];
}

void FNTR_DrawGlyph(FNTR_Fonter fonter, CNSL_Display display, CNSL_Color fg, CNSL_Color bg, int dx, int dy)
{
    int fgr = CNSL_GetRed(fg);
    int fgg = CNSL_GetGreen(fg);
    int fgb = CNSL_GetBlue(fg);
    int bgr = CNSL_GetRed(bg);
    int bgg = CNSL_GetGreen(bg);
    int bgb = CNSL_GetBlue(bg);

    int gx, gy;
    for (gy = 0; gy < FNTR_GlyphHeight(fonter); gy++) {
        for (gx = 0; gx < FNTR_GlyphWidth(fonter); gx++) {
            int level = FNTR_GlyphLevel(fonter, gx, gy);

            unsigned int red = 0xFF & ((fgr * level + bgr * (256-level))/256);
            unsigned int green = 0xFF & ((fgg * level + bgg * (256-level))/256);
            unsigned int blue = 0xFF & ((fgb * level + bgb * (256-level))/256);

            CNSL_Color c = CNSL_MakeColor(red, green, blue);
            CNSL_SetPixel(display, dx + gx, dy + gy, c);
        }
    }
}

void FNTR_DrawString(FNTR_Fonter fonter, CNSL_Display display, CNSL_Color fg, CNSL_Color bg, int x, int y, const char* str)
{
    for ( ; *str; str++) {
        FNTR_LoadGlyph(fonter, *str);
        FNTR_DrawGlyph(fonter, display, fg, bg, x, y);
        x += FNTR_GlyphWidth(fonter);
    }
}

