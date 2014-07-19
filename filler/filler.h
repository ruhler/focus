
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

#ifndef FILLER_H
#define FILLER_H

#include "consoler.h"

typedef struct {
    int width;
    int height;
    CNSL_Display display;
    CNSL_Color color;
    CNSL_Console console;
} Filler_;

typedef Filler_* Filler;

/// Filler_Create - create a filler object
///
/// Create a new filler object of given initial 'width', 'height', and filled
/// initially with 'color'. The filler object will fill the given 'console'.
///
/// Returns the created filler object, which should be freed with Filler_Free
/// when you are done with it. Returns NULL on error.
Filler Filler_Create(int width, int height, int color, CNSL_Console console);

/// Filler_Free - free a filler object
///
/// Frees the given filler object created with Filler_Create.
void Filler_Free(Filler filler);

/// Filler_Resize - resize a filler object
///
/// Change the filler object to have given 'width' and 'hegith'
void Fill_Resize(Filler filler, int width, int height);

/// Filler_FillWith - fill with the given color
///
/// Fill the display with the given color.
void Filler_FillWith(Filler filler, CNSL_Color color);

#endif//FILLER_H

