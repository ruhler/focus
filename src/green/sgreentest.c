
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

// Call this with
//  first arg: path to sgreen
//  second arg: path to filler
int main(int argc, char* argv[])
{
    const char* sgreenpath = argv[1];
    const char* fillerpath = argv[2];
    char* noargs[] = {NULL};

    CNSL_Display display = CNSL_AllocDisplay(64, 48);
    CNSL_Event event;

    setenv("CNSLSHELL", fillerpath, 1);
    fprintf(stderr, "launching sgreen\n");
    CNSL_Client sgreen = CNSL_LaunchClient(sgreenpath, noargs);

    // Verify we can interact with filler as expected.
    // first recv display is from clearing the client display
    // second recv display is from filler starting up
    fprintf(stderr, "first receive\n");
    CNSL_RecvDisplay(sgreen, display, NULL, NULL, NULL, NULL);
    fprintf(stderr, "second receive\n");
    CNSL_RecvDisplay(sgreen, display, NULL, NULL, NULL, NULL);
    fprintf(stderr, "%x\n", CNSL_GetPixel(display, 20, 30));
    assert(CNSL_GetPixel(display, 20, 30) == CNSL_MakeColor(0, 0, 0));

    event = CNSL_MakeKeypress(CNSLK_w);
    fprintf(stderr, "sending w\n");
    CNSL_SendEvent(sgreen, event);
    fprintf(stderr, "recieving\n");
    CNSL_RecvDisplay(sgreen, display, NULL, NULL, NULL, NULL);
    fprintf(stderr, "%x\n", CNSL_GetPixel(display, 20, 30));
    assert(CNSL_GetPixel(display, 20, 30) == CNSL_MakeColor(255, 255, 255));

    event = CNSL_MakeKeypress(CNSLK_r);
    fprintf(stderr, "sending r\n");
    CNSL_SendEvent(sgreen, event);
    fprintf(stderr, "recieving\n");
    CNSL_RecvDisplay(sgreen, display, NULL, NULL, NULL, NULL);
    fprintf(stderr, "%x\n", CNSL_GetPixel(display, 20, 30));
    assert(CNSL_GetPixel(display, 20, 30) == CNSL_MakeColor(255, 0, 0));

    // Launch another filler using ctrl-' c
    // Screen should go black
    CNSL_SendEvent(sgreen, CNSL_MakeKeypress(CNSLK_RCTRL));
    CNSL_SendEvent(sgreen, CNSL_MakeKeypress(CNSLK_QUOTE));
    CNSL_SendEvent(sgreen, CNSL_MakeKeyrelease(CNSLK_RCTRL));
    CNSL_SendEvent(sgreen, CNSL_MakeKeyrelease(CNSLK_QUOTE));
    CNSL_SendEvent(sgreen, CNSL_MakeKeypress(CNSLK_c));
    CNSL_SendEvent(sgreen, CNSL_MakeKeyrelease(CNSLK_c));
    CNSL_RecvDisplay(sgreen, display, NULL, NULL, NULL, NULL);
    CNSL_RecvDisplay(sgreen, display, NULL, NULL, NULL, NULL);
    assert(CNSL_GetPixel(display, 20, 30) == CNSL_MakeColor(0, 0, 0));

    // Send 'b' command to filler
    // screen should go blue
    CNSL_SendEvent(sgreen, CNSL_MakeKeypress(CNSLK_b));
    CNSL_SendEvent(sgreen, CNSL_MakeKeyrelease(CNSLK_b));
    CNSL_RecvDisplay(sgreen, display, NULL, NULL, NULL, NULL);
    assert(CNSL_GetPixel(display, 20, 30) == CNSL_MakeColor(0, 0, 0xff));

    // Switching back to window 0, screen should go back to red
    CNSL_SendEvent(sgreen, CNSL_MakeKeypress(CNSLK_RCTRL));
    CNSL_SendEvent(sgreen, CNSL_MakeKeypress(CNSLK_QUOTE));
    CNSL_SendEvent(sgreen, CNSL_MakeKeyrelease(CNSLK_RCTRL));
    CNSL_SendEvent(sgreen, CNSL_MakeKeyrelease(CNSLK_QUOTE));
    CNSL_SendEvent(sgreen, CNSL_MakeKeypress(CNSLK_0));
    CNSL_SendEvent(sgreen, CNSL_MakeKeyrelease(CNSLK_0));
    CNSL_RecvDisplay(sgreen, display, NULL, NULL, NULL, NULL);
    assert(CNSL_GetPixel(display, 20, 30) == CNSL_MakeColor(0xff, 0, 0));

    // TODO:
    //   ctrl-' o 1 (blue)
    CNSL_SendEvent(sgreen, CNSL_MakeKeypress(CNSLK_RCTRL));
    CNSL_SendEvent(sgreen, CNSL_MakeKeypress(CNSLK_QUOTE));
    CNSL_SendEvent(sgreen, CNSL_MakeKeyrelease(CNSLK_RCTRL));
    CNSL_SendEvent(sgreen, CNSL_MakeKeyrelease(CNSLK_QUOTE));
    CNSL_SendEvent(sgreen, CNSL_MakeKeypress(CNSLK_o));
    CNSL_SendEvent(sgreen, CNSL_MakeKeyrelease(CNSLK_o));
    CNSL_SendEvent(sgreen, CNSL_MakeKeypress(CNSLK_1));
    CNSL_SendEvent(sgreen, CNSL_MakeKeyrelease(CNSLK_1));
    CNSL_RecvDisplay(sgreen, display, NULL, NULL, NULL, NULL);
    assert(CNSL_GetPixel(display, 20, 30) == CNSL_MakeColor(0, 0, 0xff));

    // i g (green)
    CNSL_SendEvent(sgreen, CNSL_MakeKeypress(CNSLK_i));
    CNSL_SendEvent(sgreen, CNSL_MakeKeyrelease(CNSLK_i));
    CNSL_SendEvent(sgreen, CNSL_MakeKeypress(CNSLK_g));
    CNSL_SendEvent(sgreen, CNSL_MakeKeyrelease(CNSLK_g));
    CNSL_RecvDisplay(sgreen, display, NULL, NULL, NULL, NULL);
    fprintf(stderr, "%08x\n", CNSL_GetPixel(display, 20, 30));
    assert(CNSL_GetPixel(display, 20, 30) == CNSL_MakeColor(0, 0xff, 0));

    // q (red)
    CNSL_SendEvent(sgreen, CNSL_MakeKeypress(CNSLK_q));
    CNSL_SendEvent(sgreen, CNSL_MakeKeyrelease(CNSLK_q));
    CNSL_RecvDisplay(sgreen, display, NULL, NULL, NULL, NULL);
    assert(CNSL_GetPixel(display, 20, 30) == CNSL_MakeColor(0xff, 0, 0));

    // q
    CNSL_SendEvent(sgreen, CNSL_MakeKeypress(CNSLK_q));
    return 0;
}

