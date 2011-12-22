
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
    CNSL_Display pixels;

    // (x, y) is the coordinate in the (post scaled) image which is at the
    // upper left corner of the display. They can be negative.
    int x;
    int y;

    // The zoom factor power from the original picture size.
    // Each pixel in the original image is represented by a 
    // 2^zfp x 2^zfp square of pixels in the display.
    int zfp;
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

    int width = png_get_image_width(png_ptr, info_ptr);
    int height = png_get_image_height(png_ptr, info_ptr);
    pnger->pixels = CNSL_AllocDisplay(width, height);

    int x, y;
    for (y = 0; y < pnger->pixels.height; y++) {
        for (x = 0; x < pnger->pixels.width; x++) {
            uint8_t rc = row_pointers[y][3*x];
            uint8_t gc = row_pointers[y][3*x+1];
            uint8_t bc = row_pointers[y][3*x+2];
            CNSL_SetPixel(pnger->pixels, x, y, CNSL_MakeColor(rc, gc, bc));
        }
    }

    pnger->x = 0;
    pnger->y = 0;
    pnger->zfp = 0;

    return pnger;
}

void Pnger_Destroy(Pnger pnger)
{
    // TODO: libpng cleanup?
    CNSL_FreeDisplay(pnger->pixels);
    free(pnger);
}

void Pnger_Show(Pnger pnger, CNSL_Display display)
{
    int x = pnger->x;
    int y = pnger->y;
    int sw = pnger->pixels.width;
    int sh = pnger->pixels.height;

    // (c, r) are coordinates in the display
    int r, c;
    for (r = 0; r < display.height; r++) {
        for (c = 0; c < display.width; c++) {
            CNSL_Color color = CNSL_MakeColor(0x80, 0x80, 0x80);

            if (pnger->zfp < 0) {
                int count = 0;
                unsigned int rc = 0;
                unsigned int gc = 0;
                unsigned int bc = 0;

                int scale = 1 << (-pnger->zfp);

                int xmin = (x + c) * scale;
                int xmax = xmin + scale;
                int ymin = (y + r) * scale;
                int ymax = ymin + scale;

                if (xmin < 0) {
                    xmin = 0;
                }
                if (ymin < 0) {
                    ymin = 0;
                }

                int xs, ys;
                for (ys = ymin; ys < ymax && ys < sh; ys++) {
                    for (xs = xmin; xs < xmax && xs < sw; xs++) {
                        count++;
                        CNSL_Color pixel = CNSL_GetPixel(pnger->pixels, xs, ys);
                        rc += CNSL_GetRed8(pixel);
                        gc += CNSL_GetGreen8(pixel);
                        bc += CNSL_GetBlue8(pixel);
                    }
                }

                if (count > 0) {
                    color = CNSL_MakeColor(rc/count, gc/count, bc/count);
                }
                
            } else {
                int xsrc = (x + c) / (1 << pnger->zfp);
                int ysrc = (y + r) / (1 << pnger->zfp);

                if (ysrc >= 0 && ysrc < sh && xsrc >= 0 && xsrc < sw) {
                    color = CNSL_GetPixel(pnger->pixels, xsrc, ysrc);
                }
            }
            CNSL_SetPixel(display, c, r, color);
        }
    }
}

void Pnger_Scroll(Pnger pnger, int x, int y)
{
    pnger->x -= x;
    pnger->y -= y;
}

void Pnger_Zoom(Pnger pnger, int zfp)
{
    pnger->zfp -= zfp;
}

