
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


#include <assert.h>
#include <stdio.h>

#include "consoler.h"

int main() 
{
    const char* path = "./boxer";
    char* args[] = {"./boxer", NULL};

    CNSL_Display display = CNSL_AllocDisplay(640, 480);
    CNSL_Event event;
    CNSL_Client boxer = CNSL_LaunchClient(path, args);
    CNSL_SendEvent(boxer, CNSL_MakeResize(640, 480));

    CNSL_Color white = CNSL_MakeColor(0xff, 0xff, 0xff);
    CNSL_Color red = CNSL_MakeColor(0xff, 0x00, 0x00);
    CNSL_Color black = CNSL_MakeColor(0x00, 0x00, 0x00);
    CNSL_Color color;


    CNSL_RecvDisplay(boxer, display, NULL, NULL, NULL, NULL);
    color = CNSL_GetPixel(display, 105, 105);
    assert(color == white);

    CNSL_SendEvent(boxer, CNSL_MakeKeypress(CNSLK_j));
    CNSL_RecvDisplay(boxer, display, NULL, NULL, NULL, NULL);
    color = CNSL_GetPixel(display, 105, 105);
    assert(color == red);
    color = CNSL_GetPixel(display, 105, 115);
    assert(color == white);
    color = CNSL_GetPixel(display, 115, 105);
    assert(color == black);

    event = CNSL_MakeKeypress(CNSLK_q);
    CNSL_SendEvent(boxer, event);

    CNSL_CloseClient(boxer);
    return 0;
}

