
// Copyright (C) 2011 Richard Uhler
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

const char* FNTR_Version();

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

