
#include <stdio.h>
#include <png.h>

#include "ccl.h"

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

    Buffer display = ccl_alloc_buffer(width, height);

    unsigned int y;
    for (y = 0; y < height; y++) {
        unsigned int x;
        for (x = 0; x < width; x++) {
            unsigned int r = row_pointers[y][3*x];
            unsigned int g = row_pointers[y][3*x+1];
            unsigned int b = row_pointers[y][3*x+2];
            Color color = ccl_rgb8(r, g, b);
            ccl_setpixel(display, x, y, color);
        }
    }
    ccl_blit(display, 0, 0, 0, 0, width, height);

    // Wait for any key press to finish.
    Event event;
    ccl_event(&event);
    int sym;
    while (!ccl_keypress(&event, &sym)) {
        ccl_event(&event);
    }

    return 0;
}

