
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

#include <assert.h>
#include <stdio.h>

#include "imager.h"
#include "jpeger.h"
#include "pnger.h"

#define TYPE_UNKNOWN 0
#define TYPE_PNG 1
#define TYPE_JPEG 2

int main(int argc, char* argv[])
{
    if (argc > 1 && strcmp(argv[1], "--version") == 0) {
        printf("imager %s\n", PACKAGE_VERSION);
        return 0;
    }

    if (argc > 1 && strcmp(argv[1], "--help") == 0) {
        printf("Usage: imager -t type FILE\n");
        printf("View the png FILE\n");
        printf("\n");
        printf("Options\n");
        printf("  --help       output this help message and exit\n");
        printf("  --version    output version information and exit\n");
        printf("  -t type      the type of image being viewed. Type is 'png' or 'jpeg'\n");
        printf("\n");
        return 0;
    }

    int type = TYPE_UNKNOWN;
    if (argc < 3 || strcmp(argv[1], "-t") != 0) {
        fprintf(stderr, "missing type specification\n");
        return 1;
    }

    if (strcmp(argv[2], "png") == 0) {
        type = TYPE_PNG;
    } else if (strcmp(argv[2], "jpeg") == 0) {
        type = TYPE_JPEG;
    } else {
        fprintf(stderr, "unknown type: %s\n", argv[2]);
    }

    if (argc < 4) {
        fprintf(stderr, "no input file\n");
        return 1;
    }
    char* filename = argv[3];

    CNSL_Display pixels;
    switch (type) {
        case TYPE_PNG: pixels = Pnger_Load(filename); break;
        case TYPE_JPEG: pixels = Jpeger_Load(filename); break;
        default: assert(0 && "invalid image type");
    }

    if (!pixels.pixels) {
        fprintf(stderr, "unable to open %s\n", filename);
        return 1;
    }

    Imager imager = Imager_Create(pixels);

    int width;
    int height;

    CNSL_Event event = CNSL_RecvEvent(stdcon);
    if (!CNSL_IsResize(event, &width, &height)) {
        fprintf(stderr, "imager: expected resize event. Got %i\n", event.type);
        return 1;
    }

    CNSL_Display display = CNSL_AllocDisplay(width, height);
    Imager_Show(imager, display);
    CNSL_SendDisplay(stdcon, display, 0, 0, 0, 0, width, height);

    // Wait for q key press to finish.
    bool done = false;

    while (!done) {
        event = CNSL_RecvEvent(stdcon);
        CNSL_Keysym sym;

        if (CNSL_IsKeypress(event, &sym)) {
            switch (sym) {
                case CNSLK_q: done = true; break;

                case CNSLK_h: Imager_Scroll(imager, width/10, 0); break;
                case CNSLK_j: Imager_Scroll(imager, 0, -height/10); break;
                case CNSLK_k: Imager_Scroll(imager, 0, height/10); break;
                case CNSLK_l: Imager_Scroll(imager, -width/10, 0); break;

                case CNSLK_i: Imager_Zoom(imager, -1); break;
                case CNSLK_o: Imager_Zoom(imager, 1); break;
            }

            Imager_Show(imager, display);
            CNSL_SendDisplay(stdcon, display, 0, 0, 0, 0, width, height);

        } else if (CNSL_IsQuit(event)) {
            done = true;
        }
    }

    CNSL_FreeDisplay(pixels);
    Imager_Destroy(imager);
    return 0;
}

