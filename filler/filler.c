
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

#include "filler.h"

// fill - private method
// Fills the display with the current color and sends that out to console
void fill(Filler f)
{
    CNSL_FillRect(f->display, 0, 0, f->display.width, f->display.height, f->color);
    CNSL_SendDisplay(f->console, f->display, 0, 0, 0, 0, f->display.width, f->display.height);
}


Filler Filler_Create(int width, int height, int color, CNSL_Console console)
{
    Filler filler = malloc(sizeof(Filler_));
    if (filler == NULL) {
        return NULL;
    }

    filler->width = width;
    filler->height = height;
    filler->display = CNSL_AllocDisplay(width, height);
    filler->color = color;
    filler->console = console;

    fill(filler);
    return filler;
}

void Filler_Free(Filler filler)
{
    free(filler);
}

void Filler_Resize(Filler filler, int width, int height)
{
    filler->width = width;
    filler->height = height;
    CNSL_FreeDisplay(filler->display);
    filler->display = CNSL_AllocDisplay(width, height);
    fill(filler);
}

void Filler_FillWith(Filler filler, CNSL_Color color)
{
    filler->color = color;
    fill(filler);
}

