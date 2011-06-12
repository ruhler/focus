
#ifndef FONTER_H
#define FONTER_H

#include <fontconfig/fontconfig.h>

#include <ft2build.h>
#include FT_FREETYPE_H

#include "consoler.h"

#define MAX_FACES 4

typedef struct {
    FT_Library library;
    FT_Face face;
    int width;
    int height;
    int ascender;
    char* name;
    FT_Face faces[MAX_FACES];
} FNTR_Fonter_;

typedef FNTR_Fonter_* FNTR_Fonter;

FNTR_Fonter FNTR_Create(const char* fontname);
void FNTR_Free(FNTR_Fonter fonter);

int FNTR_MaxWidth(FNTR_Fonter fonter);
int FNTR_Height(FNTR_Fonter fonter);

void FNTR_LoadGlyph(FNTR_Fonter fonter, wchar_t c);

int FNTR_GlyphWidth(FNTR_Fonter fonter);
uint8_t FNTR_GlyphLevel(FNTR_Fonter fonter, int x, int y);

void FNTR_DrawGlyph(FNTR_Fonter fonter, CNSL_Display display,
        CNSL_Color fg, CNSL_Color bg, int x, int y);

void FNTR_DrawString(FNTR_Fonter, CNSL_Display display,
        CNSL_Color fg, CNSL_Color bg, int x, int y, const char* str);

#endif//FONTER_H

