
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <SDL/SDL.h>

#include "consoler.h"

CNSL_Client client;
SDL_Surface* screen;

CNSL_Color* toscreen(void* ud, int x, int y, int* w)
{
    CNSL_Color* pixels = (CNSL_Color*)screen->pixels;
    if (y < screen->h) {
        *w = (screen->w - x) < 0 ? 0 : (screen->w - x);
        return pixels + y*screen->w + x;
    }
    *w = 0;
    return NULL;
}

int handle_output(void* usrdata)
{
    CNSL_Display display = CNSL_AllocDisplay(screen->w, screen->h);
    while (1) {
        int x, y, w, h;
        if (CNSL_RecvDisplay(client, &x, &y, &w, &h, toscreen, NULL) == 0) {
            SDL_Event e;
            e.type = SDL_QUIT;
            SDL_PushEvent(&e);
            return 0;
        }
        SDL_UpdateRect(screen, x, y, w, h);
    }

    return 0;
}

void runserver()
{
    if (SDL_Init(SDL_INIT_VIDEO) != 0) {
        fprintf(stderr, "sdl init: %s\n", SDL_GetError());
        return;
    }
    atexit(SDL_Quit);

    screen = SDL_SetVideoMode(0, 0, 0, SDL_HWSURFACE);
    if (screen == NULL) {
        fprintf(stderr, "sdl: %s\n", SDL_GetError());
        SDL_Quit();
        return;
    }

    SDL_EnableKeyRepeat(SDL_DEFAULT_REPEAT_DELAY, SDL_DEFAULT_REPEAT_INTERVAL/2);
    SDL_ShowCursor(SDL_DISABLE);

    // Lauch a thread to handle the output.
    SDL_CreateThread(handle_output, NULL);

    // handle the input.
    SDL_Event event;

    int done = 0;
    while (!done) {
        SDL_WaitEvent(&event);
        CNSL_Event cclev;
        switch (event.type) {
            case SDL_QUIT: done = 1; break;
            case SDL_MOUSEBUTTONDOWN: done = 1; break;
            case SDL_KEYDOWN:
                if (event.key.keysym.sym == SDLK_F12) {
                    done = 1;
                } else {
                    cclev.type = CNSLE_KEYPRESS;
                    cclev.value = event.key.keysym.sym;
                    CNSL_SendEvent(client, &cclev);
                }
                break;

            case SDL_KEYUP:
                cclev.type = CNSLE_KEYRELEASE;
                cclev.value = event.key.keysym.sym;
                CNSL_SendEvent(client, &cclev);
                break;
        }
    }

    SDL_Quit();
}

int main(int argc, char* argv[])
{
    if (argc < 2) {
        fprintf(stderr, "csr prg\n");
        return 1;
    }

    int pargc = argc - 1;
    char** pargv = argv+1;

    CNSL_Init();
    client = CNSL_LaunchClient(pargv[0], pargv);
    runserver();

    CNSL_Quit();
    return 0;
}

