
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <SDL/SDL.h>

#include "consoler.h"

typedef struct {
    CNSL_Client client;
    CNSL_Display display;
    SDL_Surface* screen;
} ClientAndScreen;

int handle_output(ClientAndScreen* info)
{
    unsigned int x, y, w, h;
    bool recved = CNSL_RecvDisplay(info->client, info->display, &x, &y, &w, &h);
    while (recved) {
        SDL_UpdateRect(info->screen, x, y, w, h);
        recved = CNSL_RecvDisplay(info->client, info->display, &x, &y, &w, &h);
    }

    SDL_Event event;
    event.type = SDL_QUIT;
    SDL_PushEvent(&event);
    return 0;
}

int main(int argc, char* argv[])
{
    if (argc > 1 && strcmp(argv[1], "--version") == 0) {
        printf("sdlcsr %s\n", FOCUS_VERSION_STRING);
        return 0;
    }

    if (argc < 2) {
        fprintf(stderr, "sdlcsr client [client args ..]\n");
        return 1;
    }

    int pargc = argc - 1;
    char** pargv = argv+1;

    if (SDL_Init(SDL_INIT_VIDEO) != 0) {
        fprintf(stderr, "sdl init: %s\n", SDL_GetError());
        return 1;
    }

    ClientAndScreen info;

    info.screen = SDL_SetVideoMode(0, 0, 0, SDL_SWSURFACE);
    if (info.screen == NULL) {
        fprintf(stderr, "sdl: %s\n", SDL_GetError());
        SDL_Quit();
        return 1;
    }

    info.display.width = info.screen->w;
    info.display.height = info.screen->h;
    info.display.pixels = info.screen->pixels;

    SDL_EnableKeyRepeat(SDL_DEFAULT_REPEAT_DELAY, SDL_DEFAULT_REPEAT_INTERVAL);
    SDL_ShowCursor(SDL_DISABLE);

    // Launch the client.
    CNSL_SetGeometry(info.screen->w, info.screen->h);
    info.client = CNSL_LaunchClient(pargv[0], pargv);

    // Lauch a thread to handle the output.
    SDL_CreateThread((int (*)(void*))handle_output, &info);

    // handle the input.
    SDL_Event event;

    bool done = false;
    while (!done) {
        SDL_WaitEvent(&event);
        switch (event.type) {
            case SDL_QUIT:
                CNSL_SendEvent(info.client, CNSL_MakeQuit());
                done = true;
                break;

            case SDL_KEYDOWN:
                CNSL_SendEvent(info.client, CNSL_MakeKeypress(event.key.keysym.sym));
                break;

            case SDL_KEYUP:
                CNSL_SendEvent(info.client, CNSL_MakeKeyrelease(event.key.keysym.sym));
                break;
        }
    }

    SDL_Quit();
    return 0;
}

