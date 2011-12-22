
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

#ifndef IMAGER_H
#define IMAGER_H

#include "consoler.h"

typedef struct Imager_* Imager;

/// Imager_Create - Create an imager object
///
/// Create an imager object for the given image.
/// The returned pnger object should be freed with Imager_Destroy.
/// Returns NULL on error.
/// You should ensure pixels stays valid until you are done with the imager
/// object. Imager_Destroy will not free pixels.
Imager Imager_Create(CNSL_Display pixels);

/// Imager_Destroy - free a pnger object
///
/// Clean up a Imager object which is no longer in use.
void Imager_Destroy(Imager imager);

/// Imager_Show - show the png file
///
/// Draw the image using the Imager object view to the given display.
void Imager_Show(Imager imager, CNSL_Display display);

/// Imager_Scroll - scroll the view
///
/// Scroll the view of the image forward and to the right by the given
/// number of pixels. 'x' and/or 'y' may be negative to scroll in the reverse
/// direction.
void Imager_Scroll(Imager imager, int x, int y);


/// Imager_Zoom - zoom out by the given amount
///
/// Zoom out of the image by the given amount.
/// The amount is a factor of (2^zfp)
/// 'zfp' may be negative to zoom in.
///
/// For example, zoom(1) makes the image have as big on the display,
/// zoom(0) does nothing, zoom(-1) makes the image twice as big on the
/// display.    
void Imager_Zoom(Imager imager, int zfp);

#endif//IMAGER_H

