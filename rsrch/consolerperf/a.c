
#include <stdio.h>
#include <SDL.h>


int main(int argc, char* argv[])
{
    if (argc != 4) {
        fprintf(stderr, "usage: width height frames\n");
        return 1;
    }

    int width = atoi(argv[1]);
    int height = atoi(argv[2]);
    int frames = atoi(argv[3]);

    fprintf(stderr, "width: %i\n", width);
    fprintf(stderr, "height: %i\n", height);
    fprintf(stderr, "frames: %i\n", frames);

    if (SDL_Init(SDL_INIT_VIDEO) != 0) {
        fprintf(stderr, "sdl init: %s\n", SDL_GetError());
        return;
    }

    SDL_Surface* screen = SDL_SetVideoMode(0, 0, 0, SDL_HWSURFACE);
    if (screen == NULL) {
        fprintf(stderr, "sdl: %s\n", SDL_GetError());
        SDL_Quit();
        return;
    }

    unsigned int c1 = SDL_MapRGB(screen->format, 50, 100, 200);
    unsigned int c2 = SDL_MapRGB(screen->format, 200, 50, 150);

    SDL_Rect dst = {0, 0, width, height};

    int i;
    for (i = 0; i < frames; i++) {
        unsigned int c = i % 2 == 0 ? c1 : c2;
        SDL_FillRect(screen, &dst, c);
        SDL_UpdateRect(screen, dst.x, dst.y, dst.w, dst.h);
    }

    SDL_Quit();
    return 0;
}