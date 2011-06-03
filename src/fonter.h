
#ifndef FONTER_H
#define FONTER_H

#include <fontconfig.h>

#include <ft2build.h>
#include FT_FREETYPE_H

#include "consoler.h"

typedef struct {
    FT_Library library;
    FT_Face face;
    int height;
    int ascender;
} FNTR_Fonter_;

typedef FNTR_Fonter_* FNTR_Fonter;

// Create a fonter object for drawing with the given font name.
// Font names are fontconfig font names, 
//   for example: "Monospace-24:Bold"
// You should call FNTR_Free when you are done with the fonter object.
FNTR_Fonter FNTR_Create(const char* fontname);
void FNTR_Free(FNTR_Fonter fonter);

// Draw a string in the given font.
// The string is drawn to the display with it's lower left corner at (x,y)
void FNTR_DrawString(FNTR_Fonter, CNSL_Display display, CNSL_Color fg, CNSL_Color bg, int x, int y, const char* str);

#endif//FONTER_H

