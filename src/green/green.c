

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

GRN_Green GRN_CreateGreen()
{
    GRN_Green green = malloc(sizeof(GRN_Green_));
    if (!green) {
        return NULL;
    }

    client_id i;
    for (i = 0; i < MAX_NUM_CLIENTS; i++) {
        green->clients[i].valid = false;
    }
    green->current = -1;
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
    int width = 640;
    int height = 480;
    CNSL_GetGeometry(&width, &height);

    lock(green);

    client_id found = -1;
    client_id i;
    for (i = 0; i < MAX_NUM_CLIENTS; i++) {
        if (!green->clients[i].valid) {
            green->clients[i].client = client;
            green->clients[i].display = CNSL_AllocDisplay(width, height);
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

    // If this was the current client, we switch to a different current client
    // if possible.
    if (green->current == client) {
        green->current = -1;
        client_id i;
        for (i = 0; i < MAX_NUM_CLIENTS; i++) {
            if (green->clients[i].valid) {
                green->current = i;
                CNSL_Display d = green->clients[i].display;
                CNSL_SendDisplay(stdcon, d, 0, 0, 0, 0, d.width, d.height);
                break;
            }
        }
    }

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
        green->current = which;
        CNSL_Display d = green->clients[which].display;
        CNSL_SendDisplay(stdcon, d, 0, 0, 0, 0, d.width, d.height);
    }

    unlock(green);
}

void GRN_SendEvent(GRN_Green green, CNSL_Event event)
{
    lock(green);

    client_id i = green->current;
    if (green->clients[i].valid) {
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
        
        if (green->current == client) {
            CNSL_SendDisplay(stdcon, display, srcx, srcy, dstx, dsty, width, height);
        }
    }

    unlock(green);
}

