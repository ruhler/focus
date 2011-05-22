
#include <stdio.h>
#include <unistd.h>
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

    int stoc[2];
    int ctos[2];

    if (pipe(stoc) < 0) {
        perror("pipe");
        return 1;
    }

    if (pipe(ctos) < 0) {
        perror("pipe");
        return 1;
    }

    pid_t pid = fork();
    if (pid < 0) {
        perror("fork");
        return 1;
    }

    int fdin;
    int fdout;

    if (pid == 0) {
        if (dup2(stoc[0], STDIN_FILENO) < 0) {
            perror("dup2");
            return 1;
        }
        close(stoc[0]);
        close(stoc[1]);

        if (dup2(ctos[1], STDOUT_FILENO) < 0) {
            perror("dup2");
            return 1;
        }
        close(ctos[0]);
        close(ctos[1]);

        if (execl("./fclient", "./fclient", argv[1], argv[2], argv[3], NULL) < 0) {
            perror("exec");
            return 1;
        }

    } else {
        close(stoc[0]);
        close(ctos[1]);

        fdout = stoc[1];
        fdin = ctos[0];
    }


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

    SDL_Surface* back = SDL_CreateRGBSurface(SDL_SWSURFACE,
            width, height, 32, screen->format->Rmask, screen->format->Gmask,
            screen->format->Bmask, screen->format->Amask);

    SDL_Rect dst;
    int notdone = read(fdin, &dst, sizeof(dst));
    while (notdone) {
        int y;
        for (y = dst.y; y < dst.y + dst.h; y++) {
            read(fdin, ((unsigned int*)back->pixels) + y*back->h + dst.x, dst.w * sizeof(unsigned int));
        }
        SDL_BlitSurface(back, &dst, screen, &dst);
        SDL_UpdateRect(screen, dst.x, dst.y, dst.w, dst.h);

        notdone = read(fdin, &dst, sizeof(dst));
    }

    SDL_Quit();
    return 0;
}

