
#ifndef FONTER_H
#define FONTER_H

#include <fontconfig.h>

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

// Create a fonter object for drawing with the given font name.
// Font names are fontconfig font names, 
//   for example: "Monospace-24:Bold"
// You should call FNTR_Free when you are done with the fonter object.
FNTR_Fonter FNTR_Create(const char* fontname);
void FNTR_Free(FNTR_Fonter fonter);

// Return the width in pixels of the maximum glyph width in the font.
int FNTR_MaxWidth(FNTR_Fonter fonter);

// Return the height in pixels of the maximum glyph height in the font.
int FNTR_MaxHeight(FNTR_Fonter fonter);

// Load the given character into the current glyph slot.
void FNTR_LoadGlyph(FNTR_Fonter fonter, wchar_t c);

// Return the width of the loaded glyph
int FNTR_GlyphWidth(FNTR_Fonter fonter);

// Return the height of the loaded glyph.
int FNTR_GlyphHeight(FNTR_Fonter fonter);

// Return the grey level of the loaded glyph at the given location.
// (0, 0) is the upper left hand corner of the Char.
// Level returned is out of 256.
int FNTR_GlyphLevel(FNTR_Fonter fonter, int x, int y);

// Draw the currently loaded glyph to the given display with upper left hand
// corner at (x,y).
void FNTR_DrawGlyph(FNTR_Fonter fonter, CNSL_Display display, CNSL_Color fg, CNSL_Color bg, int x, int y);

// Draw a string in the given font.
// The string is drawn to the display with it's upper left corner at (x,y)
// This changes the currently loaded glyph.
void FNTR_DrawString(FNTR_Fonter, CNSL_Display display, CNSL_Color fg, CNSL_Color bg, int x, int y, const char* str);

#endif//FONTER_H

