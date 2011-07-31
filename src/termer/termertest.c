
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
#include <signal.h>
#include <stdio.h>

#include "consoler.h"

// Arg 1: termer program to test.
// Arg 2: termfiller program
int main(int argc, char* argv[]) 
{
    assert(argc > 2);
    const char* termerpath = argv[1];
    const char* termfillerpath = argv[2];
    char* noargs[] = {NULL};

    int width = 640;
    int height = 480;
    CNSL_Display display = CNSL_AllocDisplay(width, height);

    setenv("SHELL", termfillerpath, 1);
    setenv("TERMERFONT", "Monospace-24:Bold", 1);

    CNSL_Client termer = CNSL_LaunchClient(termerpath, noargs);
    CNSL_SendEvent(termer, CNSL_MakeResize(width, height));

    CNSL_RecvDisplay(termer, display, NULL, NULL, NULL, NULL);
    assert(CNSL_GetPixel(display, 20, 30) == CNSL_MakeColor(0, 0, 0));

    // r to go red
    CNSL_SendEvent(termer, CNSL_MakeKeypress(CNSLK_r));
    CNSL_RecvDisplay(termer, display, NULL, NULL, NULL, NULL);
    assert(CNSL_GetPixel(display, 20, 30) == CNSL_MakeColor(255, 0, 0));

    // g to go green
    CNSL_SendEvent(termer, CNSL_MakeKeypress(CNSLK_g));
    CNSL_RecvDisplay(termer, display, NULL, NULL, NULL, NULL);
    assert(CNSL_GetPixel(display, 20, 30) == CNSL_MakeColor(0, 255, 0));

    CNSL_SendEvent(termer, CNSL_MakeKeypress(CNSLK_q));

    CNSL_CloseClient(termer);
    return 0;
}

