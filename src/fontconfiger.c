
#include <stdio.h>
#include <fontconfig.h>


// Application to test out using fontconfig.
// It tries to print the font file for Monospace:Bold

int main(int argc, char* argv[])
{
    const char* name = "Monospace-24:Bold";
    if (argc > 1) {
        name = argv[1];
    }

    printf("Using font name: %s\n", name);

    FcInit();
    FcPattern* pattern = FcNameParse(name);

    FcCharSet* charset = FcCharSetCreate();
    FcCharSetAddChar(charset, 0x40);
    FcPatternAddCharSet(pattern, "charset", charset);


    FcConfigSubstitute(NULL, pattern, FcMatchPattern);
    FcDefaultSubstitute(pattern);
    FcPattern* match = FcFontMatch(NULL, pattern, NULL);

    FcValue file;
    FcPatternGet(match, "file", 0, &file);
    printf("font file: %s\n", file.u.s);

    FcValue size;
    FcPatternGet(match, "size", 0, &size);
    printf("font size: %f\n", size.u.d);

    FcValue psize;
    FcPatternGet(match, "pixelsize", 0, &psize);
    printf("font pixel size: %f\n", psize.u.d);

    FcPatternDestroy(pattern);
    FcPatternDestroy(match);

    return 0;
}

