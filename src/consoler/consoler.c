
#include <poll.h>
#include <stdlib.h>
#include <stdio.h>
#include <unistd.h>

#include "consoler.h"


// Event types
#define CNSLE_KEYPRESS 0
#define CNSLE_KEYRELEASE 1
#define CNSLE_QUIT 2

CNSL_Event CNSL_MakeKeypress(CNSL_Keysym sym)
{
    CNSL_Event event;
    event.type = CNSLE_KEYPRESS;
    event.value = sym;
    return event;
}

CNSL_Event CNSL_MakeKeyrelease(CNSL_Keysym sym)
{
    CNSL_Event event;
    event.type = CNSLE_KEYRELEASE;
    event.value = sym;
    return event;
}

CNSL_Event CNSL_MakeQuit(CNSL_Keysym sym)
{
    CNSL_Event event;
    event.type = CNSLE_QUIT;
    event.value = 0;
    return event;
}

bool CNSL_IsKeypress(CNSL_Event event, CNSL_Keysym* sym)
{
    if (event.type == CNSLE_KEYPRESS) {
        *sym = event.value;
    }
    return event.type == CNSLE_KEYPRESS;
}

bool CNSL_IsKeyrelease(CNSL_Event event, CNSL_Keysym* sym)
{
    if (event.type == CNSLE_KEYRELEASE) {
        *sym = event.value;
    }
    return event.type == CNSLE_KEYRELEASE;
}

bool CNSL_IsQuit(CNSL_Event event)
{
    return event.type == CNSLE_QUIT;
}



uint8_t CNSL_GetRed8(CNSL_Color c)
{
    return 0xFF & (c >> 16);
}


uint8_t CNSL_GetBlue8(CNSL_Color c)
{
    return 0xFF & c;
}

uint8_t CNSL_GetGreen8(CNSL_Color c)
{
    return 0xFF & (c >> 8);
}

uint32_t CNSL_GetRGB8(CNSL_Color c)
{
    return c;
}

CNSL_Color CNSL_MakeColor(uint8_t r, uint8_t g, uint8_t b)
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

        CNSL_Client c = malloc(sizeof(CNSL_Client_));
        if (!c) {
            return NULL;
        }

        c->fdout = stoc[1];
        c->fdin = ctos[0];
        return c;
    }

    return NULL;
}

void CNSL_CloseClient(CNSL_Client client)
{
    close(client->fdin);
    close(client->fdout);
    free(client);
}

int CNSL_RecvEvent(CNSL_Console console, CNSL_Event* event)
{
    int red = read(console->fdin, event, sizeof(CNSL_Event));
    return red;
}

int CNSL_SendEvent(CNSL_Client client, const CNSL_Event* event)
{
    write(client->fdout, event, sizeof(CNSL_Event));
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
    write(console->fdout, header, 4*sizeof(unsigned int));

    int y;
    for (y = srcy; y < height+srcy; y++) {
        CNSL_Color* line = display->pixels + (y * display->width) + srcx;
        write(console->fdout, line, width*sizeof(CNSL_Color));
    }
    return 1;
}


int CNSL_RecvDisplay(CNSL_Client client,
        unsigned int* dstx, unsigned int* dsty,
        unsigned int* width, unsigned int* height,
        CNSL_RDFunction f, void* ud)
{
    static CNSL_Color* junk = NULL;
    static junkwidth = 0;

    unsigned int header[4] = {0};
    if (read(client->fdin, header, 4 * sizeof(unsigned int)) < 4) {
        return 0;
    }

    *dstx = header[0];
    *dsty = header[1];
    *width = header[2];
    *height = header[3];

    if (*width > junkwidth) {
        // We introduce a small memory leak here. I think it's okay.
        junk = realloc(junk, (*width) * sizeof(CNSL_Color));
        junkwidth = *width;
    }

    int y;
    for (y = *dsty; y < *dsty + *height; y++) {
        int wmax = 0;
        CNSL_Color* buf = f(ud, *dstx, y, &wmax);

        int wuse = wmax < *width ? wmax : *width;
        int wleft = *width - wuse;

        read(client->fdin, buf, wuse * sizeof(CNSL_Color));
        read(client->fdin, junk, wleft * sizeof(CNSL_Color));
    }

    return 1;
}

CNSL_Color* CNSL_RDToDisplay(void* ud, int x, int y, int* w)
{
    CNSL_Display dsp = (CNSL_Display)ud;
    if (y < dsp->height) {
        *w = (dsp->width - x) < 0 ? 0 : (dsp->width - x);
        return dsp->pixels + dsp->width * y + x;
    }
    *w = 0;
    return NULL;
}

int CNSL_PollDisplay(CNSL_Client client)
{
    struct pollfd fd;
    fd.fd = client->fdin;
    fd.events = POLLIN;
    return poll(&fd, 1, 0);
}

int CNSL_Init()
{
    stdcon = (CNSL_Console)malloc(sizeof(CNSL_Console_));
    if (!stdcon) {
        return -1;
    }
    stdcon->fdin = STDIN_FILENO;
    stdcon->fdout = STDOUT_FILENO;

    return 1;
}

int CNSL_Quit()
{
    free(stdcon);
    return 1;
}

void CNSL_GetGeometry(int* width, int* height)
{
    const char* widthenv = getenv("CNSLWIDTH");
    const char* heightenv = getenv("CNSLHEIGHT");

    if (widthenv) {
        *width = atoi(widthenv);
    }

    if (heightenv) {
        *height = atoi(heightenv);
    }
}

void CNSL_SetGeometry(int width, int height)
{
    char widthstr[10] = {0};
    char heightstr[10] = {0};
    snprintf(widthstr, 10, "%i", width);
    snprintf(heightstr, 10, "%i", height);
    setenv("CNSLWIDTH", widthstr, 1);
    setenv("CNSLHEIGHT", heightstr, 1);
}

