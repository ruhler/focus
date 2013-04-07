
// Copyright (C) 2011 Richard Uhler <ruhler@member.fsf.org>
//
// This file is part of Focus.
//
// Focus is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.
//
// Focus is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with Focus.  If not, see <http://www.gnu.org/licenses/>.

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

const char* FNTR_Version()
{
    return FOCUS_VERSION_STRING;
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
    FcResult result;
    FcPattern* match = FcFontMatch(NULL, pattern, &result);

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

    // Freetype says the metrics can be off by up to a pixel, and we have had
    // trouble with the height in the past (see 1.1.0/15.tsk), so we'll add 1
    // to it just to be safe.
    fonter->width = from26_6(fonter->face->size->metrics.max_advance);
    fonter->height = 1+from26_6(fonter->face->size->metrics.height);
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
	FcResult result;
        FcPattern* match = FcFontMatch(NULL, pattern, &result);

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
    int bgr = CNSL_GetRed8(bg);
    int bgg = CNSL_GetGreen8(bg);
    int bgb = CNSL_GetBlue8(bg);
    int dr = CNSL_GetRed8(fg) - bgr;
    int dg = CNSL_GetGreen8(fg) - bgg;
    int db = CNSL_GetBlue8(fg) - bgb;

    int cell_width = FNTR_GlyphWidth(fonter);
    int cell_height = FNTR_Height(fonter);
    CNSL_FillRect(display, dx, dy, cell_width, cell_height, bg);

    int left = fonter->face->glyph->bitmap_left;
    int width = fonter->face->glyph->bitmap.width;
    int top = fonter->ascender - fonter->face->glyph->bitmap_top;
    int height = fonter->face->glyph->bitmap.rows;
    uint8_t* bitmap = fonter->face->glyph->bitmap.buffer;

    // Make sure we won't try to draw outside the cell_width or cell_height.
    // This might otherwise happen because of the inaccuracy in freetype's
    // reported metrics.
    int bxmin = (left < 0) ? -left : 0;
    int bymin = (top < 0) ? -top : 0;
    int bxmax = (left + width > cell_width) ? cell_width - left : width;
    int bymax = (top + height > cell_height) ? cell_height - top : height;

    int bx, by;
    for (by = bymin; by < bymax; by++) {
        for (bx = bxmin; bx < bxmax; bx++) {
            int level = bitmap[by * width + bx];

            CNSL_Color c = CNSL_MakeColor(
                    bgr + ((level * dr) >> 8),
                    bgg + ((level * dg) >> 8),
                    bgb + ((level * db) >> 8));

            CNSL_SetPixel(display, dx + left + bx, dy + top + by, c);
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

