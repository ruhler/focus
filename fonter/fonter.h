
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

/// FNTR_Version - get the version of the fonter library
///
/// Return a string describing the version of the fonter library.
const char* FNTR_Version();

/// FNTR_Create - create a fonter object
///
/// Create a new fonter object which holds the font described by fontname.
/// fontname is a fontconfig font name, such as `Monospace-24:Bold`. See the
/// fontconfig documentation for more details on font names.
FNTR_Fonter FNTR_Create(const char* fontname);

/// FNTR_Free - free a fonter object
///
/// Free a fonter object which is no longer needed.
void FNTR_Free(FNTR_Fonter fonter);

/// FNTR_MaxWidth - get max width of glyph in font
///
/// Return the width in pixels of the maximum glyph width in the font.
int FNTR_MaxWidth(FNTR_Fonter fonter);

/// FNTR_Height - get height of glyphs in font
///
/// Return the height in pixels of glyphs in the font. It is assumed all
/// glyphs have the same height.
int FNTR_Height(FNTR_Fonter fonter);

/// FNTR_LoadGlyph - load a glyph
///
/// Load the given character into the current glyph slot of the fonter object.
void FNTR_LoadGlyph(FNTR_Fonter fonter, wchar_t c);

/// FNTR_GlyphWidth - get a glyph width
///
/// Return the width in pixels of the loaded glyph.  It's undefined what
/// happens if no glyph is loaded.
int FNTR_GlyphWidth(FNTR_Fonter fonter);

/// FNTR_GlyphLevel - get a grey level from a glyph
///
/// Return the grey level of the loaded glyph at the given location. The level
/// ranges from 0 to 255 inclusive.  Coordinate (0,0) is the upper left hand
/// corner of the glyph, with x increasing to the left and y increasing down.
uint8_t FNTR_GlyphLevel(FNTR_Fonter fonter, int x, int y);

/// FNTR_DrawGlyph - draw a glyph to a display
///
/// Draw the currently loaded glyph to the given display with the upper left
/// hand corner of the glyph at (x, y) of the display. The color fg is used
/// for the foreground color, and bg used for the background color of the
/// glyph drawn.
void FNTR_DrawGlyph(FNTR_Fonter fonter, CNSL_Display display,
        CNSL_Color fg, CNSL_Color bg, int x, int y);

/// FNTR_DrawString - Draw a string to a display
///
/// Draw the given string to the display with the upper left hand corner of
/// the string drawn to position (x,y) of the display. This overwrites the
/// currently loaded glyph.
void FNTR_DrawString(FNTR_Fonter, CNSL_Display display,
        CNSL_Color fg, CNSL_Color bg, int x, int y, const char* str);

#endif//FONTER_H

