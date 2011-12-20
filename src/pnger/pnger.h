
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

#endif//PNGER_H

