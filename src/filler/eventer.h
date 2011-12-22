
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

#ifndef EVENTER_H
#define EVENTER_H

#include "consoler.h"

struct EventerNode;
typedef struct EventerNode** Eventer;

/// Eventer_Create - create an eventer object
///
/// Create a new eventer object for mapping events to actions. Returns the
/// created eventer object, which should be freed with Eventer_Free when you
/// are done with it. Returns NULL on error.
Eventer Eventer_Create();

/// Eventer_Free - free an eventer object.
/// 
/// Frees the given eventer object created with Eventer_Create.
void Eventer_Free(Eventer eventer);

/// Eventer_Insert - add an event to the eventer object
///
/// Insert an event mapping into the given eventer object.
/// 'action' is a user interpreted string which will be returned by the
/// lookup function. It is expected this will be a tcl command.
///
/// The inserted mapping overwrites any conflicting existing mappings in the
/// event object.
void Eventer_Insert(Eventer eventer, CNSL_Event event, const char* action);

/// Eventer_Lookup - lookup up an event in the eventer object
///
/// Look up the action for the given event in the eventer object. Returns NULL
/// if no mapping is found for the event.
const char* Eventer_Lookup(Eventer eventer, CNSL_Event event);

#endif//EVENTER_H

