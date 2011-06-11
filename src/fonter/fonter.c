
#include <math.h>

#include "fonter.h"

// Round a fixed point number in 26.6 format to an integer.
int from26_6(int x)
{
    int w = x >> 6;
    if (x & 0x3F) {
        w++;
    }
    return w;
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

    fonter->name = strdup(fontname);

    int i;
    for (i = 1; i < MAX_FACES; i++) {
        fonter->faces[i] = NULL;
    }
    fonter->faces[0] = fonter->face;
    return fonter;
}

void FNTR_Free(FNTR_Fonter fonter)
{
    FT_Done_FreeType(fonter->library);
    free(fonter->name);
    free(fonter);
}

int FNTR_MaxWidth(FNTR_Fonter fonter)
{
    return fonter->width;
}

int FNTR_Height(FNTR_Fonter fonter)
{
    return fonter->height;
}

void FNTR_LoadGlyph(FNTR_Fonter fonter, wchar_t c)
{
    FT_UInt index = 0;
    fonter->face = fonter->faces[0];

    int i;
    for (i = 0; i < MAX_FACES && fonter->faces[i]; i++) {
        index = FT_Get_Char_Index(fonter->faces[i], c);
        if (index != 0) {
            fonter->face = fonter->faces[i];
            break;
        }
    }

    if (i < MAX_FACES && fonter->faces[i] == NULL) {
        // We don't have a face with this character. Try to find one.
        FcPattern* pattern = FcNameParse(fonter->name);
        FcCharSet* charset = FcCharSetCreate();
        FcCharSetAddChar(charset, c);
        FcPatternAddCharSet(pattern, "charset", charset);
        FcConfigSubstitute(NULL, pattern, FcMatchPattern);
        FcDefaultSubstitute(pattern);
        FcPattern* match = FcFontMatch(NULL, pattern, NULL);

        FcValue file;
        FcValue psize;
        FcPatternGet(match, "file", 0, &file);
        FcPatternGet(match, "pixelsize", 0, &psize);

        int size = (int)ceil(psize.u.d);

        FT_New_Face(fonter->library, file.u.s, 0, &fonter->faces[i]);
        FcPatternDestroy(pattern);
        FcPatternDestroy(match);

        FT_Set_Pixel_Sizes(fonter->faces[i], size, size);
        index = FT_Get_Char_Index(fonter->faces[i], c);
        fonter->face = fonter->faces[i];
    }

    FT_Load_Glyph(fonter->face, index, FT_LOAD_RENDER);
}

int FNTR_GlyphWidth(FNTR_Fonter fonter)
{
    return from26_6(fonter->face->glyph->metrics.horiAdvance);
}

uint8_t FNTR_GlyphLevel(FNTR_Fonter fonter, int x, int y)
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

void FNTR_DrawGlyph(FNTR_Fonter fonter, CNSL_Display display,
        CNSL_Color fg, CNSL_Color bg, int dx, int dy)
{
    int fgr = CNSL_GetRed8(fg);
    int fgg = CNSL_GetGreen8(fg);
    int fgb = CNSL_GetBlue8(fg);
    int bgr = CNSL_GetRed8(bg);
    int bgg = CNSL_GetGreen8(bg);
    int bgb = CNSL_GetBlue8(bg);

    int gx, gy;
    for (gy = 0; gy < FNTR_Height(fonter); gy++) {
        for (gx = 0; gx < FNTR_GlyphWidth(fonter); gx++) {
            uint8_t level = FNTR_GlyphLevel(fonter, gx, gy);

            unsigned int red = 0xFF & ((fgr * level + bgr * (256-level))/256);
            unsigned int green = 0xFF & ((fgg * level + bgg * (256-level))/256);
            unsigned int blue = 0xFF & ((fgb * level + bgb * (256-level))/256);

            CNSL_Color c = CNSL_MakeColor(red, green, blue);
            CNSL_SetPixel(display, dx + gx, dy + gy, c);
        }
    }
}

void FNTR_DrawString(FNTR_Fonter fonter, CNSL_Display display,
        CNSL_Color fg, CNSL_Color bg, int x, int y, const char* str)
{
    for ( ; *str; str++) {
        FNTR_LoadGlyph(fonter, *str);
        FNTR_DrawGlyph(fonter, display, fg, bg, x, y);
        x += FNTR_GlyphWidth(fonter);
    }
}

