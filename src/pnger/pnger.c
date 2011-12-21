
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
#include <png.h>

#include "pnger.h"

struct Pnger_ {
    uint8_t** data;
    unsigned int width;
    unsigned int height;

    // (x, y) is the coordinate in the (post scaled) image which is at the
    // upper left corner of the display. They can be negative.
    int x;
    int y;
};

Pnger Pnger_Create(const char* filename)
{
    FILE* pngfile = fopen(filename, "rb");
    if (!pngfile) {
        fprintf(stderr, "unable to open %s\n", filename);
        return NULL;
    }

    png_structp png_ptr = png_create_read_struct(
        PNG_LIBPNG_VER_STRING, NULL, NULL, NULL
    );

    if (!png_ptr) {
        fprintf(stderr, "unable to create png read struct.\n");
        return NULL;
    }

    png_infop info_ptr = png_create_info_struct(png_ptr);
    if (!info_ptr)
    {
        png_destroy_read_struct(&png_ptr, (png_infopp)NULL, (png_infopp)NULL);
        fprintf(stderr, "unable to create png info struct.\n");
        return NULL;
    }

    png_init_io(png_ptr, pngfile);
    unsigned int transforms = 
          PNG_TRANSFORM_STRIP_16 | PNG_TRANSFORM_STRIP_ALPHA
        | PNG_TRANSFORM_PACKING | PNG_TRANSFORM_GRAY_TO_RGB
        | PNG_TRANSFORM_EXPAND;

    png_read_png(png_ptr, info_ptr, transforms, NULL);

    uint8_t** row_pointers = png_get_rows(png_ptr, info_ptr);

    Pnger pnger = malloc(sizeof(struct Pnger_));
    pnger->data = row_pointers;
    pnger->width = png_get_image_width(png_ptr, info_ptr);
    pnger->height = png_get_image_height(png_ptr, info_ptr);
    pnger->x = 0;
    pnger->y = 0;

    return pnger;
}

void Pnger_Destroy(Pnger pnger)
{
    // TODO: libpng cleanup?
    free(pnger);
}

void Pnger_Show(Pnger pnger, CNSL_Display display)
{
    int x = pnger->x;
    int y = pnger->y;
    int sw = pnger->width;
    int sh = pnger->height;

    // (c, r) are coordinates in the display
    int r, c;
    for (r = 0; r < display.height; r++) {
        for (c = 0; c < display.width; c++) {
            if (y + r >= 0 && y + r < sh && x + c >= 0 && x + c < sw) {
                uint8_t rc = pnger->data[y+r][3*(x+c)];
                uint8_t gc = pnger->data[y+r][3*(x+c)+1];
                uint8_t bc = pnger->data[y+r][3*(x+c)+2];
                CNSL_Color color = CNSL_MakeColor(rc, gc, bc);
                CNSL_SetPixel(display, c, r, color);
            } else {
                // Background color: grey
                CNSL_SetPixel(display, c, r, CNSL_MakeColor(0x80, 0x80, 0x80));
            }
        }
    }
}

void Pnger_Scroll(Pnger pnger, int x, int y)
{
    pnger->x -= x;
    pnger->y -= y;
}

