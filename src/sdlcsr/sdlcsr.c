
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

    if (argc > 1 && strcmp(argv[1], "--help") == 0) {
        printf("Usage: sdlcsr client [args ...]\n");
        printf("Launch a consoler client under SDL\n");
        printf("\n");
        printf("Options\n");
        printf("  --help       output this help message and exit\n");
        printf("  --version    output version information and exit\n");
        printf("\n");
        return 0;
    }

    if (argc < 2) {
        fprintf(stderr, "sdlcsr: missing client\n");
        fprintf(stderr, "Try `sdlcsr --help' for more information.\n");
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

