
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

#include <stdio.h>

#include "consoler.h"
#include "fonter.h"

int main()
{
    const char* fontname = "Monospace-25:Bold";
    const char* string = "Hello there! goodbye!";

    int width = 640;
    int height = 480;
    CNSL_GetGeometry(&width, &height);

    CNSL_Display display = CNSL_AllocDisplay(width, height);

    CNSL_Event event;
    CNSL_Keysym sym;
    bool done = false;

    CNSL_Color fg = CNSL_MakeColor(0xff, 0xff, 0xff);
    CNSL_Color bg = CNSL_MakeColor(0x00, 0x00, 0xff);

    int x = 30;
    int y = 40;

    FNTR_Fonter fonter = FNTR_Create(fontname);
    FNTR_DrawString(fonter, display, fg, bg, x, y, string);

    while (!done) {
        CNSL_SendDisplay(stdcon, display, 0, 0, 0, 0, width, height);

        event = CNSL_RecvEvent(stdcon);
        if (CNSL_IsQuit(event) || (CNSL_IsKeypress(event, &sym) && sym == CNSLK_q)) {
            done = true;
            break;
        }
    }

    FNTR_Free(fonter);
    return 0;
}

