
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

/// Pnger_Load - load a png file
///
/// Create a new CNSL_Display initialized with the contents of the given png
/// file. The display should be freed using CNSL_FreeDisplay when you are done
/// using it.
CNSL_Display Pnger_Load(const char* filename);

#endif//PNGER_H

