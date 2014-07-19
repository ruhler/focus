
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

int main(int argc, char* argv[])
{
    assert(CNSL_Version() != NULL);

    CNSL_Keysym sym;
    CNSL_Event event;
   
    event = CNSL_MakeKeypress(CNSLK_a);
    assert(CNSL_IsKeypress(event, &sym));
    assert(!CNSL_IsKeyrelease(event, NULL));
    assert(!CNSL_IsQuit(event));
    assert(sym == CNSLK_a);

    event = CNSL_MakeKeyrelease(CNSLK_b);
    assert(!CNSL_IsKeypress(event, NULL));
    assert(CNSL_IsKeyrelease(event, &sym));
    assert(!CNSL_IsQuit(event));
    assert(sym == CNSLK_b);

    event = CNSL_MakeQuit();
    assert(!CNSL_IsKeypress(event, NULL));
    assert(!CNSL_IsKeyrelease(event, NULL));
    assert(CNSL_IsQuit(event));

    CNSL_Color color = CNSL_MakeColor(0x12, 0x34, 0x56);
    assert(CNSL_GetRed8(color) == 0x12);
    assert(CNSL_GetGreen8(color) == 0x34);
    assert(CNSL_GetBlue8(color) == 0x56);
    assert(CNSL_GetRGB8(color) == 0x123456);

    printf("all tests passed\n");
    return 0;
}

