
#include <stdio.h>
#include <png.h>

#include "consoler.h"

int main(int argc, char* argv[])
{
    if (argc < 2) {
        fprintf(stderr, "no input file\n");
        return 1;
    }
    char* pngfilename = argv[1];

    FILE* pngfile = fopen(pngfilename, "rb");
    if (!pngfile) {
        fprintf(stderr, "unable to open %s\n", pngfilename);
        return 1;
    }

    png_structp png_ptr = png_create_read_struct(
        PNG_LIBPNG_VER_STRING, NULL, NULL, NULL
    );

    if (!png_ptr) {
        fprintf(stderr, "unable to create png read struct.\n");
        return 1;
    }

    png_infop info_ptr = png_create_info_struct(png_ptr);
    if (!info_ptr)
    {
        png_destroy_read_struct(&png_ptr, (png_infopp)NULL, (png_infopp)NULL);
        fprintf(stderr, "unable to create png info struct.\n");
        return 1;
    }

    png_init_io(png_ptr, pngfile);
    unsigned int transforms = 
          PNG_TRANSFORM_STRIP_16 | PNG_TRANSFORM_STRIP_ALPHA
        | PNG_TRANSFORM_PACKING | PNG_TRANSFORM_GRAY_TO_RGB
        | PNG_TRANSFORM_EXPAND;

    png_read_png(png_ptr, info_ptr, transforms, NULL);

    unsigned char** row_pointers;
    row_pointers = png_get_rows(png_ptr, info_ptr);

    // Assuming now we are in RGB format.
    unsigned int width = png_get_image_width(png_ptr, info_ptr);
    unsigned int height = png_get_image_height(png_ptr, info_ptr);

    CNSL_Init();
    CNSL_Display display = CNSL_AllocDisplay(width, height);

    unsigned int y;
    for (y = 0; y < height; y++) {
        unsigned int x;
        for (x = 0; x < width; x++) {
            unsigned int r = row_pointers[y][3*x];
            unsigned int g = row_pointers[y][3*x+1];
            unsigned int b = row_pointers[y][3*x+2];
            CNSL_Color color = CNSL_MakeColor(r, g, b);
            CNSL_SetPixel(display, x, y, color);
        }
    }
    CNSL_SendDisplay(stdcon, display, 0, 0, 0, 0, width, height);

    // Wait for q key press to finish.
    CNSL_Event event;
    CNSL_RecvEvent(stdcon, &event);
    int sym;
    while (!(CNSL_IsKeypress(&event, &sym) && sym == CNSLK_q)) {
        CNSL_RecvEvent(stdcon, &event);
    }

    CNSL_Quit();
    return 0;
}

