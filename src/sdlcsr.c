
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <SDL/SDL.h>

#include "ccl.h"
#include "csr.h"

Client client;
SDL_Surface* screen;

void pixel(int x, int y, int r, int g, int b)
{
    if (x >= 0 && x <= screen->w && y >= 0 && y <= screen->h) {
        SDL_Rect dst = {x, y, 1, 1};
        SDL_FillRect(screen, &dst, SDL_MapRGB(screen->format, r, g, b));
    }
}

void pixel_(void* ud, int x, int y, Color c)
{
    pixel(x, y, ccl_redof(c), ccl_greenof(c), ccl_blueof(c));
}

int handle_output(void* usrdata)
{
    Buffer display = ccl_alloc_buffer(screen->w, screen->h);
    while (1) {
        int x, y, w, h;
        if (csr_update(client, &x, &y, &w, &h, pixel_, NULL) == 0) {
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

    screen = SDL_SetVideoMode(1280, 800, 0, SDL_ANYFORMAT);
    if (screen == NULL) {
        fprintf(stderr, "sdl: %s\n", SDL_GetError());
        SDL_Quit();
        return;
    }

    // Lauch a thread to handle the output.
    SDL_CreateThread(handle_output, NULL);

    // handle the input.
    SDL_Event event;

    int done = 0;
    while (!done) {
        SDL_WaitEvent(&event);
        Event cclev;
        switch (event.type) {
            case SDL_QUIT: done = 1; break;
            case SDL_MOUSEBUTTONDOWN: done = 1; break;
            case SDL_KEYDOWN:
                if (event.key.keysym.sym == SDLK_F12) {
                    done = 1;
                } else {
                    cclev.type = EVENT_KEYPRESS;
                    cclev.value = event.key.keysym.sym;
                    csr_event(client, cclev);
                }
                break;

            case SDL_KEYUP:
                cclev.type = EVENT_KEYRELEASE;
                cclev.value = event.key.keysym.sym;
                csr_event(client, cclev);
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

    client = csr_launch(pargv[0], pargv);
    runserver();
    return 0;
}

