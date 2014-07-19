
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
    // green.png is a png file which is solid green and sized 64x128
    char* path = "./imager";
    char* green = "green.png";

    CNSL_Display display = CNSL_AllocDisplay(64, 128);
    char* args[] = {path, "-t", "png", green, NULL};

    CNSL_Client pnger = CNSL_LaunchClient(path, args);
    CNSL_SendEvent(pnger, CNSL_MakeResize(64, 128));

    CNSL_RecvDisplay(pnger, display, NULL, NULL, NULL, NULL);
    assert(CNSL_GetPixel(display, 20, 30) == CNSL_MakeColor(0, 0xff, 0));

    CNSL_SendEvent(pnger, CNSL_MakeKeypress(CNSLK_q));
    CNSL_CloseClient(pnger);
    return 0;
}

