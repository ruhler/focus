
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

#ifndef PNGER_H
#define PNGER_H

#include "consoler.h"

typedef struct Pnger_* Pnger;

/// Pnger_Create - Create a pnger object
///
/// Create a pnger object with the png file specified by the given filename.
/// The returned pnger object should be freed with Pnger_Destroy.
/// Returns NULL on error.
Pnger Pnger_Create(const char* filename);

/// Pnger_Destroy - free a pnger object
///
/// Clean up a Pnger object which is no longer in use.
void Pnger_Destroy(Pnger pnger);

/// Pnger_Show - show the png file
///
/// Draw the png file using the Pnger object view to the given display.
void Pnger_Show(Pnger pnger, CNSL_Display display);

/// Pnger_Scroll - scroll the view
///
/// Scroll the view of the png file forward and to the right by the given
/// number of pixels. 'x' and/or 'y' may be negative to scroll in the reverse
/// direction.
void Pnger_Scroll(Pnger pnger, int x, int y);


/// Pnger_Zoom - zoom out by the given amount
///
/// Zoom out of the png image by the given amount.
/// The amount is a factor of (2^zfp)
/// 'zfp' may be negative to zoom in.
///
/// For example, zoom(1) makes the png image have as big on the display,
/// zoom(0) does nothing, zoom(-1) makes the png image twice as big on the
/// display.    
void Pnger_Zoom(Pnger pnger, int zfp);

#endif//PNGER_H

