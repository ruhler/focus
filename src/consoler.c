
#include <stdlib.h>
#include <unistd.h>

#include "consoler.h"


int CNSL_GetRed(CNSL_Color c)
{
    return 0xFF & (c >> 16);
}


int CNSL_GetBlue(CNSL_Color c)
{
    return 0xFF & c;
}

int CNSL_GetGreen(CNSL_Color c)
{
    return 0xFF & (c >> 8);
}

CNSL_Color CNSL_MakeColor(int r, int g, int b)
{
    return (r << 16) | (g << 8) | b;
}

CNSL_Display CNSL_AllocDisplay(unsigned int width, unsigned int height) {
    CNSL_Display display = malloc(sizeof(CNSL_Display_));
    if (!display) {
        return NULL;
    }

    display->width = width;
    display->height = height;
    display->pixels = malloc(sizeof(CNSL_Color) * width * height);
    if (!display->pixels) {
        free(display);
        return NULL;
    }
    return display;
}


void CNSL_FreeDisplay(CNSL_Display display)
{
    if (display) {
        free(display->pixels);
        free(display);
    }
}

CNSL_Color CNSL_GetPixel(CNSL_Display display, unsigned int x, unsigned int y)
{
    return display->pixels[y*display->width + x];
}

void CNSL_SetPixel(CNSL_Display display, unsigned int x, unsigned int y, CNSL_Color color)
{
    display->pixels[y*display->width + x] = color;
}

int CNSL_IsKeypress(const CNSL_Event* e, int* code)
{
    if (e->type == CNSLE_KEYPRESS) {
        *code = e->value;
    }
    return e->type == CNSLE_KEYPRESS;
}

int CNSL_IsKeyrelease(const CNSL_Event* e, int* code)
{
    if (e->type == CNSLE_KEYRELEASE) {
        *code = e->value;
    }
    return e->type == CNSLE_KEYRELEASE;
}

CNSL_Console stdcon = NULL;


CNSL_Client CNSL_LaunchClient(const char* path, char* const args[])
{
    int stoc[2];
    int ctos[2];

    if (pipe(stoc) < 0) {
        perror("pipe");
        return NULL;
    }

    if (pipe(ctos) < 0) {
        perror("pipe");
        return NULL;
    }

    pid_t pid = fork();
    if (pid < 0) {
        perror("fork");
        return NULL;
    }

    if (pid == 0) {
        if (dup2(stoc[0], STDIN_FILENO) < 0) {
            perror("dup2");
            return NULL;
        }
        close(stoc[0]);
        close(stoc[1]);

        if (dup2(ctos[1], STDOUT_FILENO) < 0) {
            perror("dup2");
            return NULL;
        }
        close(ctos[0]);
        close(ctos[1]);

        if (execvp(path, args) < 0) {
            perror("execvp");
            return NULL;
        }

    } else {
        close(stoc[0]);
        close(ctos[1]);

        int fdw = stoc[1];
        int fdr = ctos[0];

        CNSL_Client c = malloc(sizeof(CNSL_Client_));
        if (!c) {
            return NULL;
        }
        
        c->fout = fdopen(fdw, "w");
        c->fin = fdopen(fdr, "r");
        if (!c->fout || !c->fin) {
            free(c);
            return NULL;
        }

        return c;
    }

    return NULL;
}

void CNSL_CloseClient(CNSL_Client client)
{
    fclose(client->fin);
    fclose(client->fout);
    free(client);
}

int CNSL_RecvEvent(CNSL_Console console, CNSL_Event* event)
{
    if (feof(console->fin)) {
        return 0;
    }

    fread(event, sizeof(CNSL_Event), 1, stdin);
}

int CNSL_SendEvent(CNSL_Client client, const CNSL_Event* event)
{
    fwrite(&event, sizeof(CNSL_Event), 1, client->fout);
    fflush(client->fout);
    return 1;
}


int CNSL_SendDisplay(CNSL_Console console, CNSL_Display display,
        unsigned int srcx, unsigned int srcy,
        unsigned int dstx, unsigned int dsty,
        unsigned int width, unsigned int height)
{
    // Output the data using the following format:
    // ui4: x destination
    // ui4: y destination
    // ui4: width
    // ui4: height
    // width*ui4: row 1
    // width*ui4: row 2
    // ...
    // width*ui4: row (height)

    unsigned int header[] = {dstx, dsty, width, height};
    fwrite(header, 4, 4, console->fout);

    int x;
    int y;
    for (y = srcy; y < height+srcy; y++) {
        for (x = srcx; x < width + srcx; x++) {
            CNSL_Color c = CNSL_GetPixel(display, x, y);
            fwrite(&c, 4, 1, console->fout);
        }
    }
    fflush(console->fout);
    return 1;
}


int CNSL_RecvDisplay(CNSL_Client client,
        unsigned int* dstx, unsigned int* dsty,
        unsigned int* width, unsigned int* height,
        CNSL_RDFunction f, void* ud)
{
    unsigned int header[4] = {0};
    if (fread(header, 4, 4, client->fin) < 4) {
        return 0;
    }

    *dstx = header[0];
    *dsty = header[1];
    *width = header[2];
    *height = header[3];
    int r = 0;
    int c = 0;
    for (r = 0; r < *height; r++) {
        for (c = 0; c < *width; c++) {
            CNSL_Color col;
            fread(&col, 4, 1, client->fin);
            f(ud, c+*dstx, r+*dsty, col);
        }
    }

    return 1;

}

int CNSL_Init()
{
    stdcon = (CNSL_Console)malloc(sizeof(CNSL_Console_));
    if (!stdcon) {
        return -1;
    }
    stdcon->fin = stdin;
    stdcon->fout = stdout;

    return 1;
}

int CNSL_Quit()
{
    free(stdcon);
    return 1;
}

