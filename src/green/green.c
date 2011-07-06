

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

#include <stdlib.h>
#include <stdio.h>

#include "green.h"

void lock(GRN_Green green)
{
    pthread_mutex_lock(&green->mutex);
}

void unlock(GRN_Green green)
{
    pthread_mutex_unlock(&green->mutex);
}

// Show the given client.
// It is expected this client is newly shown. This will send a resize event to
// the client if needed, and update the display appropriately given the
// current single/split state.
// Requires locking in the caller.
void ShowClientRL(GRN_Green green, client_id id)
{
    if (green->mode == GRN_MODE_SINGLE) {
        if (id == green->current[GRN_FOCUS_TOP]) {
            if (green->clients[id].width != green->width || green->clients[id].height != green->height) {
                CNSL_SendEvent(green->clients[id].client, CNSL_MakeResize(green->width, green->height));
                green->clients[id].width = green->width;
                green->clients[id].height = green->height;
            }

            CNSL_Display display = green->clients[id].display;
            CNSL_SendDisplay(stdcon, display, 0, 0, 0, 0, green->width, green->height);
        }
        return;
    }
    
    if (green->current[GRN_FOCUS_TOP] == id) {
        if (green->clients[id].width != green->width || green->clients[id].height != green->height/2) {
            CNSL_SendEvent(green->clients[id].client, CNSL_MakeResize(green->width, green->height/2));
            green->clients[id].width = green->width;
            green->clients[id].height = green->height/2;
        }

        CNSL_Display display = green->clients[id].display;
        CNSL_SendDisplay(stdcon, display, 0, 0, 0, 0, green->width, green->height/2);
    }

    if (green->current[GRN_FOCUS_BOTTOM] == id) {
        if (green->clients[id].width != green->width || green->clients[id].height != green->height/2) {
            CNSL_SendEvent(green->clients[id].client, CNSL_MakeResize(green->width, green->height/2));
            green->clients[id].width = green->width;
            green->clients[id].height = green->height/2;
        }

        CNSL_Display display = green->clients[id].display;
        CNSL_SendDisplay(stdcon, display, 0, 0, 0, green->height/2, green->width, green->height/2);
    }
}


// Find and display clients to display in the window if needed.
// This will check if the current displays are valid and do the appropriate
// thing.
// Requires locking in the caller.
void ChooseClientsRL(GRN_Green green)
{
    client_id ti = green->current[GRN_FOCUS_TOP];
    if (ti == -1 || !green->clients[ti].valid) {
        green->current[GRN_FOCUS_TOP] = -1;
        client_id i;
        for (i = 0; i < MAX_NUM_CLIENTS; i++) {
            if (green->clients[i].valid) {
                green->current[GRN_FOCUS_TOP] = i;
                ShowClientRL(green, i);
                break;
            }
        }
    } else {
        // If we are splitting, the top could be full screen and still need
        // redrawing because it should no longer be full screen.
        ShowClientRL(green, ti);
    }

    if (green->mode == GRN_MODE_SPLIT) {
        client_id bi = green->current[GRN_FOCUS_BOTTOM];
        if (bi == -1 || !green->clients[bi].valid) {
            green->current[GRN_FOCUS_BOTTOM] = -1;
            client_id i;
            for (i = 0; i < MAX_NUM_CLIENTS; i++) {
                if (green->clients[i].valid) {
                    green->current[GRN_FOCUS_BOTTOM] = i;
                    ShowClientRL(green, i);
                    break;
                }
            }
        }
    }
}

GRN_Green GRN_CreateGreen(int width, int height)
{
    GRN_Green green = malloc(sizeof(GRN_Green_));
    if (!green) {
        return NULL;
    }

    client_id i;
    for (i = 0; i < MAX_NUM_CLIENTS; i++) {
        green->clients[i].valid = false;
    }
    green->current[GRN_FOCUS_TOP] = -1;
    green->current[GRN_FOCUS_BOTTOM] = -1;
    green->mode = GRN_MODE_SINGLE;
    green->focus = GRN_FOCUS_TOP;
    green->width = width;
    green->height = height;
    pthread_mutex_init(&green->mutex, NULL);

    return green;
}

void GRN_FreeGreen(GRN_Green green)
{
    lock(green);

    client_id i;
    for (i = 0; i < MAX_NUM_CLIENTS; i++) {
        if (green->clients[i].valid) {
            CNSL_FreeDisplay(green->clients[i].display);
        }
    }

    unlock(green);
    pthread_mutex_destroy(&green->mutex);
    free(green);
}

client_id GRN_AddClient(GRN_Green green, CNSL_Client client)
{
    lock(green);

    int width = green->width;
    int height = green->height;

    client_id found = -1;
    client_id i;
    for (i = 0; i < MAX_NUM_CLIENTS; i++) {
        if (!green->clients[i].valid) {
            green->clients[i].client = client;
            green->clients[i].display = CNSL_AllocDisplay(width, height);
            green->clients[i].width = 0;
            green->clients[i].height = 0;
            green->clients[i].valid = true;
            found = i;
            break;
        }
    }

    unlock(green);
    return found;
}

void GRN_RemoveClient(GRN_Green green, client_id client)
{
    lock(green);

    if (green->clients[client].valid) {
        CNSL_FreeDisplay(green->clients[client].display);
        green->clients[client].valid = false;
    }

    ChooseClientsRL(green);

    unlock(green);
}

bool GRN_HasClients(GRN_Green green)
{
    bool found = false;
    lock(green);

    client_id i;
    for (i = 0; i < MAX_NUM_CLIENTS; i++) {
        if (green->clients[i].valid) {
            found = true;
            break;
        }
    }

    unlock(green);
    return found;
}

void GRN_ChangeCurrent(GRN_Green green, client_id which)
{
    lock(green);

    if (green->clients[which].valid) {
        green->current[green->focus] = which;
        ShowClientRL(green, which);
    }

    unlock(green);
}

void GRN_Split(GRN_Green green)
{
    lock(green);

    if (green->mode != GRN_MODE_SPLIT) {
        green->mode = GRN_MODE_SPLIT;
        ChooseClientsRL(green);
    }

    unlock(green);
}

void GRN_Unsplit(GRN_Green green)
{
    lock(green);

    if (green->mode != GRN_MODE_SINGLE) {
        green->mode = GRN_MODE_SINGLE;
        green->focus = GRN_FOCUS_TOP;
        green->current[GRN_FOCUS_BOTTOM] = -1;
        ShowClientRL(green, green->current[GRN_FOCUS_TOP]);
    }

    unlock(green);
}

void GRN_SetFocus(GRN_Green green, GRN_Focus focus)
{
    lock(green);

    if (green->mode == GRN_MODE_SPLIT) {
        green->focus = focus;
    }

    unlock(green);
}

void GRN_SendEvent(GRN_Green green, CNSL_Event event)
{
    lock(green);

    client_id i = green->current[green->focus];
    if (i != -1 && green->clients[i].valid) {
        CNSL_SendEvent(green->clients[i].client, event);
    }

    unlock(green);
}

void GRN_SendDisplay(GRN_Green green, client_id client, CNSL_Display display,
        unsigned int srcx, unsigned int srcy,
        unsigned int dstx, unsigned int dsty,
        unsigned int width, unsigned int height)
{
    lock(green);

    if (green->clients[client].valid) {
        int x, y;
        for (y = 0; y < height; y++) {
            for (x = 0; x < width; x++) {
                CNSL_Color c = CNSL_GetPixel(display, x+srcx, y+srcy);
                CNSL_SetPixel(green->clients[client].display, x+srcx, y+srcy, c);
            }
        }
        
        // TODO: make sure we don't go off the bottom of the screen.
        if (green->mode == GRN_MODE_SINGLE && green->current[GRN_FOCUS_TOP] == client) {
            CNSL_SendDisplay(stdcon, display, srcx, srcy, dstx, dsty, width, height);
        } else if (green->mode == GRN_MODE_SPLIT) {
            if (green->current[GRN_FOCUS_TOP] == client) {
                CNSL_SendDisplay(stdcon, display, srcx, srcy, dstx, dsty, width, height);
            }
            
            if (green->current[GRN_FOCUS_BOTTOM] == client) {
                CNSL_SendDisplay(stdcon, display, srcx, srcy, dstx, dsty + green->height/2, width, height);
            }
        }
    }

    unlock(green);
}

