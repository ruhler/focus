
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
#include <stdlib.h>
#include <jpeglib.h>

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
        printf("jpeger %s\n", FOCUS_VERSION_STRING);
        return 0;
    }

    if (argc > 1 && strcmp(argv[1], "--help") == 0) {
        printf("Usage: jpeger FILE\n");
        printf("View the jpeg FILE\n");
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
    char* jpegfilename = argv[1];

    FILE* jpegfile = fopen(jpegfilename, "rb");
    if (!jpegfile) {
        fprintf(stderr, "unable to open %s\n", jpegfilename);
        return 1;
    }

    struct jpeg_decompress_struct cinfo;
    struct jpeg_error_mgr jerr;
    cinfo.err = jpeg_std_error(&jerr);
    jpeg_create_decompress(&cinfo);
    jpeg_stdio_src(&cinfo, jpegfile);
    jpeg_read_header(&cinfo, TRUE);
    jpeg_start_decompress(&cinfo);

    unsigned int width = cinfo.output_width;
    unsigned int height = cinfo.output_height;

    CNSL_Display display = CNSL_AllocDisplay(width, height);
    uint8_t* data = malloc(width * 3);

    unsigned int y;
    for (y = 0; y < height; y++) {
        jpeg_read_scanlines(&cinfo, &data, 1);
        unsigned int x;
        for (x = 0; x < width; x++) {
            uint8_t r = data[3*x];
            uint8_t g = data[3*x+1];
            uint8_t b = data[3*x+2];
            CNSL_Color color = CNSL_MakeColor(r, g, b);
            CNSL_SetPixel(display, x, y, color);
        }
    }

    jpeg_finish_decompress(&cinfo);
    jpeg_destroy_decompress(&cinfo);
    fclose(jpegfile);
    free(data);

    CNSL_SendDisplay(stdcon, display, 0, 0, 0, 0, width, height);

    // Wait for q key press to finish.
    CNSL_Event event = CNSL_RecvEvent(stdcon);
    while (!shouldquit(event)) {
        event = CNSL_RecvEvent(stdcon);
    }

    return 0;
}

