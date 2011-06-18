
#ifndef CONSOLER_H
#define CONSOLER_H

#include <stdbool.h>
#include <stdint.h>

#include "consoler_keysym.h"

const char* CNSL_Version();

typedef struct {
    int type;
    int value;
} CNSL_Event;

CNSL_Event CNSL_MakeKeypress(CNSL_Keysym sym);
CNSL_Event CNSL_MakeKeyrelease(CNSL_Keysym sym);
CNSL_Event CNSL_MakeQuit();

bool CNSL_IsKeypress(CNSL_Event event, CNSL_Keysym* sym);
bool CNSL_IsKeyrelease(CNSL_Event event, CNSL_Keysym* sym);
bool CNSL_IsQuit(CNSL_Event event);

typedef uint32_t CNSL_Color;

uint8_t CNSL_GetRed8(CNSL_Color c);
uint8_t CNSL_GetBlue8(CNSL_Color c);
uint8_t CNSL_GetGreen8(CNSL_Color c);
uint32_t CNSL_GetRGB8(CNSL_Color c);

CNSL_Color CNSL_MakeColor(uint8_t r, uint8_t g, uint8_t b);


typedef struct {
    unsigned int width;
    unsigned int height;
    CNSL_Color* pixels;
} CNSL_Display;

CNSL_Display CNSL_AllocDisplay(unsigned int width, unsigned int height);
void CNSL_FreeDisplay(CNSL_Display display);

CNSL_Color CNSL_GetPixel(CNSL_Display display, unsigned int x, unsigned int y);
void CNSL_SetPixel(CNSL_Display display, unsigned int x, unsigned int y, CNSL_Color color);


typedef struct {
    int fdin;
    int fdout;
} CNSL_Console;

extern CNSL_Console stdcon;

typedef struct {
    int fdin;
    int fdout;
} CNSL_Client;

CNSL_Client CNSL_LaunchClient(const char* path, char* const args[]);
void CNSL_CloseClient(CNSL_Client client);

CNSL_Event CNSL_RecvEvent(CNSL_Console console);
bool CNSL_SendEvent(CNSL_Client client, CNSL_Event event);

void CNSL_SendDisplay(CNSL_Console console, CNSL_Display display,
        unsigned int srcx, unsigned int srcy,
        unsigned int dstx, unsigned int dsty,
        unsigned int width, unsigned int height);

bool CNSL_RecvDisplay(CNSL_Client client, CNSL_Display display,
        unsigned int* dstx, unsigned int* dsty,
        unsigned int* width, unsigned int* height);

void CNSL_GetGeometry(int* width, int* height);
void CNSL_SetGeometry(int width, int height);


#endif//CONSOLER_H

