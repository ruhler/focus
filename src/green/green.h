
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

#ifndef GREEN_H
#define GREEN_H

#include <pthread.h>

#include "consoler.h"

#define MAX_NUM_CLIENTS 10

typedef int client_id;

typedef struct {
    bool valid;
    int width;
    int height;
    CNSL_Client client;
    CNSL_Display display;
} GRN_ClientInfo;

typedef enum {
    GRN_MODE_SINGLE,
    GRN_MODE_SPLIT,
} GRN_Mode;

typedef enum {
    GRN_FOCUS_TOP = 0,
    GRN_FOCUS_BOTTOM = 1,
} GRN_Focus;

// The shared data structure.
//  clients:: array of client info.
//  mode:: Whether we are in single mode or split.
//  focus:: Which window has focus in split mode.
//  current:: ids of the currently displayed clients (or -1).
//  width:: width of the view
//  height:: height of the view
//  mutex:: lock for all fields of this structure.
//
// CNSL_Clients are owned by the user, so we don't have to worry about
// allocating or cleaning up after them.
// CNSL_Displays for the clients are allocated for each new client, and must
// be deallocated for each removed client.
typedef struct {
    GRN_ClientInfo clients[MAX_NUM_CLIENTS];
    GRN_Mode mode;
    GRN_Focus focus;
    client_id current[2];
    int width;
    int height;
    pthread_mutex_t mutex;
} GRN_Green_;

typedef GRN_Green_* GRN_Green;

/// GRN_CreateGreen - create a green object
///
/// Initializes a green object. The object should be freed with the 
/// +GRN_FreeGreen+ function when you are done with it.
///
/// width:: the width of the view in pixels
/// height:: the height of the view in pixels
///
/// Returns NULL on error.
GRN_Green GRN_CreateGreen(int width, int height);

/// GRN_FreeGreen - free a created green object
///
/// Free the given green object created with +GRN_CreateGreen+.
/// It's probably a bad idea if some threads are still using the green object
/// when you go to free it.
void GRN_FreeGreen(GRN_Green green);

/// GRN_AddClient - add a new client
///
/// Adds a new client. Returns the id of the newly added client, or -1 on
/// error. Does not switch to the client.
client_id GRN_AddClient(GRN_Green green, CNSL_Client client);

/// GRN_RemoveClient - remove a client
///
/// Remove a client. Does nothing if the client doesn't exist.
/// If this is the current client, another client is picked to be the current
/// client after the given client is removed.
void GRN_RemoveClient(GRN_Green green, client_id client);

/// GRN_HasClients - return true if there are clients
///
/// Returns true if there are any valid clients in the green object, false
/// otherwise.
bool GRN_HasClients(GRN_Green green);

/// GRN_ChangeCurrent - change the current client
///
/// Change to the client with id 'which'. If that client is not valid, nothing
/// happens.
void GRN_ChangeCurrent(GRN_Green green, client_id which);

/// GRN_Split - split the window
///
/// Split the window into two. This does nothing if the window is already
/// split.
void GRN_Split(GRN_Green green);

/// GRN_Unsplit - unsplit the window
///
/// Unsplit the window, restoring the window with focus as a single client.
/// This does nothing if the window is not currently split.
void GRN_Unsplit(GRN_Green green);

/// GRN_Focus - change the window with focus
/// 
/// Does nothing if not in split mode
void GRN_SetFocus(GRN_Green green, GRN_Focus focus);

/// GRN_SendEvent - send an event to the current client
///
/// Sends an event to the current client.
///
/// Does nothing if there is no current client.
void GRN_SendEvent(GRN_Green green, CNSL_Event event);

/// GRN_SendDisplay - send client display to server
///
/// Update the green display for the given client. If the given client is the
/// current client, this updates the display, otherwise this does nothing.
void GRN_SendDisplay(GRN_Green green, client_id client, CNSL_Display display,
        unsigned int srcx, unsigned int srcy,
        unsigned int dstx, unsigned int dsty,
        unsigned int width, unsigned int height);

#endif//GREEN_H
