
#include <assert.h>
#include <stdio.h>

#include "fonter.h"

int main()
{
    assert(FNTR_Version() != NULL);

    int ret = 0;
    const char* fontname = "Monospace-24:Bold";
    const char* string = "Hello there!";

    FNTR_Fonter fonter = FNTR_Create(fontname);
    FNTR_LoadGlyph(fonter, '_');

    // For a monospace font, we expect the max width and glyph width to be the
    // same.
    if (FNTR_MaxWidth(fonter) != FNTR_GlyphWidth(fonter)) {
       fprintf(stderr, "max width not same as glyph width for monospace font!"); 
       fprintf(stderr, "max = %i, glyph = %i\n", FNTR_MaxWidth(fonter), FNTR_GlyphWidth(fonter)); 
       ret = 1;
    }

    FNTR_Free(fonter);

    const char* fontname2 = "Monospace-30:Bold";

    FNTR_Fonter fonter2 = FNTR_Create(fontname2);
    FNTR_LoadGlyph(fonter2, '_');

    if (FNTR_MaxWidth(fonter2) != FNTR_GlyphWidth(fonter2)) {
       fprintf(stderr, "max width not same as glyph width for monospace font!"); 
       fprintf(stderr, "max = %i, glyph = %i\n", FNTR_MaxWidth(fonter2), FNTR_GlyphWidth(fonter2)); 
       ret = 1;
    }

    return ret;
}

