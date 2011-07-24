
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

#ifndef DISPLAY_H
#define DISPLAY_H

#include "consoler.h"
#include "fonter.h"

#include "screen.h"

typedef struct {
    CNSL_Display display;

    FNTR_Fonter fonter;
    int cell_width;
    int cell_height;

    int columns;
    int lines;

    // bounds of cells which have changed and need redisplay
    // -1 means they have not been set yet.
    int maxcol;
    int mincol;
    int maxrow;
    int minrow;

} DISPLAY_Display_;

typedef DISPLAY_Display_* DISPLAY_Display;

/// DISPLAY_Alloc - allocate a display
///
/// Returns the allocated display, or NULL if there was a problem allocating
/// the display.
///
/// The display should be freed by a call to DISPLAY_Free when you are done
/// using it.
DISPLAY_Display DISPLAY_Alloc();

/// DISPLAY_Free - free a display
///
/// Free a display allocated with DISPLAY_Alloc.
void DISPLAY_Free(DISPLAY_Display display);

/// DISPLAY_Lines - get number of lines in display
///
/// Returns the number of character lines that fit in the display.
int DISPLAY_Lines(DISPLAY_Display display);

/// DISPLAY_Columns - get number of columns in display
///
/// Returns the number of character columns that fit in the display.
int DISPLAY_Columns(DISPLAY_Display display);

/// DISPLAY_DrawCell - draw a character on the display
///
/// Draw the given 'cell' on the display at 'pos'.
void DISPLAY_DrawCell(DISPLAY_Display display,
        SCREEN_Position pos, const SCREEN_Cell* cell);

/// DISPLAY_Show - send display to consoler server.
///
/// The DISPLAY_DrawCell function updates the local version of the display.
/// For the user to see the updated version of the local display this function
/// must be called.
void DISPLAY_Show(DISPLAY_Display display);

/// DISPLAY_Resize - resize the display
///
/// Resize the display to the given width and height in pixels.
void DISPLAY_Resize(DISPLAY_Display display, int width, int height);

#endif//DISPLAY_H

