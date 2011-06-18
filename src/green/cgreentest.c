
// Copyright (C) 2011 Richard Uhler
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
#include <unistd.h>

#include "consoler.h"

void launch(const char* path, char* args[])
{
    pid_t pid = fork();
    if (pid < 0) {
        perror("fork");
        assert(false);
    } else if (pid == 0) {
        execv(path, args);
    }
}

// Call this with
//  first arg: path to sgreen
//  second arg: path to cgreen
//  third arg: path to filler
int main(int argc, char* argv[])
{
    char* sgreenpath = argv[1];
    char* cgreenpath = argv[2];
    char* fillerpath = argv[3];
    char socketname[35];
    snprintf(socketname, 35, "/tmp/cgreentest.%i", getpid());
    char* sgreenargs[] = {sgreenpath, "-s", socketname, NULL};
    char* cgreenargs[] = {cgreenpath, fillerpath, NULL};

    CNSL_Display display = CNSL_AllocDisplay(64, 48);
    CNSL_Event event;

    setenv("CNSLSHELL", fillerpath, 1);
    CNSL_Client sgreen = CNSL_LaunchClient(sgreenpath, sgreenargs);

    // Make the shell filler white
    CNSL_RecvDisplay(sgreen, display, NULL, NULL, NULL, NULL);
    CNSL_RecvDisplay(sgreen, display, NULL, NULL, NULL, NULL);
    CNSL_SendEvent(sgreen, CNSL_MakeKeypress(CNSLK_w));
    CNSL_RecvDisplay(sgreen, display, NULL, NULL, NULL, NULL);
    assert(CNSL_GetPixel(display, 20, 30) == CNSL_MakeColor(255, 255, 255));

    // Launch a new filler via cgreen
    setenv("GREENSVR", socketname, 1);
    launch(cgreenpath, cgreenargs);

    // we clear the new client display, then the new filler makes it black.
    CNSL_RecvDisplay(sgreen, display, NULL, NULL, NULL, NULL);
    CNSL_RecvDisplay(sgreen, display, NULL, NULL, NULL, NULL);
    assert(CNSL_GetPixel(display, 20, 30) == CNSL_MakeColor(0, 0, 0));
    CNSL_SendEvent(sgreen, CNSL_MakeKeypress(CNSLK_r));
    CNSL_RecvDisplay(sgreen, display, NULL, NULL, NULL, NULL);
    assert(CNSL_GetPixel(display, 20, 30) == CNSL_MakeColor(255, 0, 0));

    // quit both fillers
    CNSL_SendEvent(sgreen, CNSL_MakeKeypress(CNSLK_q));
    CNSL_SendEvent(sgreen, CNSL_MakeKeypress(CNSLK_q));
    return 0;
}

