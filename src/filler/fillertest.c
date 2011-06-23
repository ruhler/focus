
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

void sigpipehandler(int signum)
{
    fprintf(stderr, "sigpipehandler");
    assert(false && "sigpipe thrown!");
}

// Call this with first command line argument the path to the filler
// application to test.
int main(int argc, char* argv[]) 
{
    // We've had problems with SIGPIPEs being thrown, ending the test case,
    // but not having it fail. Make sure that doesn't happen.
    struct sigaction action;
    action.sa_handler = &sigpipehandler;
    sigfillset(&action.sa_mask);
    action.sa_flags = 0;
    int sar = sigaction(SIGPIPE, &action, NULL);
    assert(sar == 0);

    CNSL_Display display = CNSL_AllocDisplay(640, 480);
    CNSL_Event event;

    CNSL_Client filler = CNSL_LaunchClient(argv[1], argv+1);

    CNSL_RecvDisplay(filler, display, NULL, NULL, NULL, NULL);
    printf("%x\n", CNSL_GetPixel(display, 20, 30));
    assert(CNSL_GetPixel(display, 20, 30) == CNSL_MakeColor(0, 0, 0));

    event = CNSL_MakeKeypress(CNSLK_w);
    CNSL_SendEvent(filler, event);
    CNSL_RecvDisplay(filler, display, NULL, NULL, NULL, NULL);
    printf("%x\n", CNSL_GetPixel(display, 20, 30));
    assert(CNSL_GetPixel(display, 20, 30) == CNSL_MakeColor(255, 255, 255));

    event = CNSL_MakeKeypress(CNSLK_r);
    CNSL_SendEvent(filler, event);
    CNSL_RecvDisplay(filler, display, NULL, NULL, NULL, NULL);
    printf("%x\n", CNSL_GetPixel(display, 20, 30));
    assert(CNSL_GetPixel(display, 20, 30) == CNSL_MakeColor(255, 0, 0));

    event = CNSL_MakeKeypress(CNSLK_q);
    CNSL_SendEvent(filler, event);

    // We had a bug where sending to a client which has closed causes a
    // SIGPIPE and kills the sending process. Let's make sure that doesn't
    // happen.
    CNSL_SendEvent(filler, CNSL_MakeQuit());
    sleep(1);
    CNSL_SendEvent(filler, CNSL_MakeQuit());

    CNSL_CloseClient(filler);
    return 0;
}

