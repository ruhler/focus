
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <SDL.h>

int fdw;
int fdr;
SDL_Surface* screen;

void clear()
{
    SDL_FillRect(screen, NULL, SDL_MapRGB(screen->format, 0x30, 0x30, 0x30));
}

void pixel(int x, int y, int r, int g, int b)
{
    SDL_Rect dst = {x, y, 1, 1};
    SDL_FillRect(screen, &dst, SDL_MapRGB(screen->format, r, g, b));
}

int handle_output(void* usrdata)
{
    FILE* pfr = fdopen(fdr, "r");
    if (!pfr) {
        perror("fdopen");
        return -1;
    }

    while (1) {
        char cmd = fgetc(pfr);
        switch (cmd) {
            case EOF:
                // ignore?
                break;

            case 'C':
                clear();
                break;

            case 'P':
            {
                unsigned int x = 0;
                unsigned int y = 0;
                unsigned int r = 0;
                unsigned int g = 0;
                unsigned int b = 0;
                if (fscanf(pfr, "@%x,%x:%x%x%x", &x, &y, &r, &g, &b) < 5) {
                    fprintf(stderr, "error reading P command\n");
                    fprintf(stderr, " x=%i, y=%i, r=%i, g=%i, b=%i\n");
                }
                fprintf(stderr, "pixel %i, %i, %i, %i, %i\n", x, y, r, g, b);
                pixel(x, y, r, g, b);

            }

            default:
                fprintf(stderr, "unknown command: %c(%02X)\n", cmd, cmd);
                break;
        }
        SDL_UpdateRect(screen, 0, 0, 0, 0);
    }
}

void runserver()
{
    FILE* pfw = fdopen(fdw, "w");
    if (!pfw) {
        perror("fdopen");
        return;
    }

    if (SDL_Init(SDL_INIT_VIDEO) != 0) {
        fprintf(stderr, "sdl init: %s\n", SDL_GetError());
        return;
    }

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
        switch (event.type) {
            case SDL_QUIT: done = 1; break;
            case SDL_MOUSEBUTTONDOWN: done = 1; break;
            case SDL_KEYDOWN:
                if (event.key.keysym.sym == SDLK_F12) {
                    done = 1;
                } else {
                    fprintf(pfw, "P%02X", event.key.keysym.scancode);
                    fflush(pfw);
                }
                break;

            case SDL_KEYUP:
                fprintf(pfw, "R%02X", event.key.keysym.scancode);
                fflush(pfw);
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
    const char* prg = argv[1];

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
        close(stoc[0]);
        close(stoc[1]);

        if (execl(prg, prg, NULL) < 0) {
            perror("execl");
            return 1;
        }

    } else {
        fdw = stoc[1];
        //close(stoc[0]);
        
        fdr = ctos[0];
        //close(ctos[1]);

        runserver();
    }
    return 0;
}

