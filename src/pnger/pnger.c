
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

#include <stdio.h>
#include <png.h>

#include "consoler.h"

bool shouldquit(CNSL_Event event)
{
    if (CNSL_IsQuit(event)) {
        return true;
    }

    CNSL_Keysym sym;
    return (CNSL_IsKeypress(event, &sym) && sym == CNSLK_q);
}

int main(int argc, char* argv[])
{
    if (argc > 1 && strcmp(argv[1], "--version") == 0) {
        printf("pnger %s\n", FOCUS_VERSION_STRING);
        return 0;
    }

    if (argc > 1 && strcmp(argv[1], "--help") == 0) {
        printf("Usage: pnger FILE\n");
        printf("View the png FILE\n");
        printf("\n");
        printf("Options\n");
        printf("  --help       output this help message and exit\n");
        printf("  --version    output version information and exit\n");
        printf("\n");
        return 0;
    }

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

    uint8_t** row_pointers = png_get_rows(png_ptr, info_ptr);

    // Assuming now we are in RGB format.
    unsigned int width = png_get_image_width(png_ptr, info_ptr);
    unsigned int height = png_get_image_height(png_ptr, info_ptr);

    CNSL_Display display = CNSL_AllocDisplay(width, height);

    unsigned int y;
    for (y = 0; y < height; y++) {
        unsigned int x;
        for (x = 0; x < width; x++) {
            uint8_t r = row_pointers[y][3*x];
            uint8_t g = row_pointers[y][3*x+1];
            uint8_t b = row_pointers[y][3*x+2];
            CNSL_Color color = CNSL_MakeColor(r, g, b);
            CNSL_SetPixel(display, x, y, color);
        }
    }
    CNSL_SendDisplay(stdcon, display, 0, 0, 0, 0, width, height);

    // Wait for q key press to finish.
    CNSL_Event event = CNSL_RecvEvent(stdcon);
    while (!shouldquit(event)) {
        event = CNSL_RecvEvent(stdcon);
    }

    return 0;
}

