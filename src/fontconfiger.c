
#include <stdio.h>
#include <fontconfig.h>


// Application to test out using fontconfig.
// It tries to print the font file for Monospace:Bold

int main()
{
    FcInit();
    FcPattern* pattern = FcNameParse("Monospace:Bold");
    FcConfigSubstitute(NULL, pattern, FcMatchPattern);
    FcDefaultSubstitute(pattern);
    FcPattern* match = FcFontMatch(NULL, pattern, NULL);

    FcValue file;
    FcPatternGet(match, "file", 0, &file);
    printf("font file: %s\n", file.u.s);

    FcPatternDestroy(pattern);
    FcPatternDestroy(match);

    return 0;
}

