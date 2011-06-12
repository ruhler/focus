
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
    char* sgreenargs[] = {sgreenpath, "-s", "cgreentest.socket", NULL};
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
    setenv("GREENSVR", "cgreentest.socket", 1);
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

