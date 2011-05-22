
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

    unsigned int c1 = 0x003264c8;
    unsigned int c2 = 0x00c83296;

    unsigned int* pixels = malloc(width * height * sizeof(unsigned int));

    int i;
    for (i = 0; i < frames; i++) {
        unsigned int c = i % 2 == 0 ? c1 : c2;
        int x, y;
        for (y = 0; y < height; y++) {
            for (x = 0; x < width; x++) {
                pixels[y*width + x] = c;
            }
        }

        SDL_Rect dst = {0, 0, width, height};
        write(STDOUT_FILENO, &dst, sizeof(dst));
        for (y = 0; y < height; y++) {
            write(STDOUT_FILENO, pixels + (y*width) + x, width * sizeof(unsigned int));
        }
    }

    return 0;
}

