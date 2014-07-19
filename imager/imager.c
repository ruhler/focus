
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

#include "imager.h"

struct Imager_ {
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

Imager Imager_Create(CNSL_Display pixels)
{
    Imager imager = malloc(sizeof(struct Imager_));
    imager->pixels = pixels;
    imager->x = 0;
    imager->y = 0;
    imager->zfp = 0;
    return imager;
}

void Imager_Destroy(Imager imager)
{
    free(imager);
}

void Imager_Show(Imager imager, CNSL_Display display)
{
    int x = imager->x;
    int y = imager->y;
    int sw = imager->pixels.width;
    int sh = imager->pixels.height;

    // (c, r) are coordinates in the display
    int r, c;
    for (r = 0; r < display.height; r++) {
        for (c = 0; c < display.width; c++) {
            CNSL_Color color = CNSL_MakeColor(0x80, 0x80, 0x80);

            if (imager->zfp < 0) {
                int count = 0;
                unsigned int rc = 0;
                unsigned int gc = 0;
                unsigned int bc = 0;

                int scale = 1 << (-imager->zfp);

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
                        CNSL_Color pixel = CNSL_GetPixel(imager->pixels, xs, ys);
                        rc += CNSL_GetRed8(pixel);
                        gc += CNSL_GetGreen8(pixel);
                        bc += CNSL_GetBlue8(pixel);
                    }
                }

                if (count > 0) {
                    color = CNSL_MakeColor(rc/count, gc/count, bc/count);
                }
                
            } else {
                int xsrc = (x + c) / (1 << imager->zfp);
                int ysrc = (y + r) / (1 << imager->zfp);

                if (ysrc >= 0 && ysrc < sh && xsrc >= 0 && xsrc < sw) {
                    color = CNSL_GetPixel(imager->pixels, xsrc, ysrc);
                }
            }
            CNSL_SetPixel(display, c, r, color);
        }
    }
}

void Imager_Scroll(Imager imager, int x, int y)
{
    imager->x -= x;
    imager->y -= y;
}

void Imager_Zoom(Imager imager, int zfp)
{
    imager->zfp -= zfp;
}

