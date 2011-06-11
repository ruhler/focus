
#ifndef CONSOLER_H
#define CONSOLER_H

#include <stdbool.h>
#include <stdint.h>

#include "consoler_keysym.h"

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

// Arguments are same as for exec.
CNSL_Client CNSL_LaunchClient(const char* path, char* const args[]);
void CNSL_CloseClient(CNSL_Client client);

// Get the next event from the console.
// Returns zero if no more events, -1 on error, nonzero on success.
int CNSL_RecvEvent(CNSL_Console console, CNSL_Event* event);

// Send an event to a client
// Returns -1 on error, nonzero on success.
// 0 on no more client?
int CNSL_SendEvent(CNSL_Client client, const CNSL_Event* event);

// Send a display from the client to the server.
// srcx and srcy specify where in display to start taking pixels from.
// width and height specify the dimensions of the pixel block to send.
// dstx and dsty are the destination for the block on the Console's display
// for this client.
int CNSL_SendDisplay(CNSL_Console console, CNSL_Display display,
        unsigned int srcx, unsigned int srcy,
        unsigned int dstx, unsigned int dsty,
        unsigned int width, unsigned int height);

// Get a screen update from the client.
// This is a blocking call.
// Calls function f to get a buffer for each row to copy into.
//  x, y are the position of the left pixel of the row in the display
//  w should be filled in with the size (in pixels) of the returned buffer.
// Returns the updated rectangle via x, y, w, and h.
// Returns nonzero on success, zero on end of file, -1 on error.
typedef CNSL_Color* (*CNSL_RDFunction)(void* ud, int x, int y, int* w);
int CNSL_RecvDisplay(CNSL_Client client,
        unsigned int* dstx, unsigned int* dsty,
        unsigned int* width, unsigned int* height,
        CNSL_RDFunction f, void* ud);

// Function to pass to CNSL_RecvDisplay which loads the display into another
// CNSL_Display. The CNSL_Display to load into should be given as the user
// data, and display->width should be used for max width.
CNSL_Color* CNSL_RDToDisplay(void* ud, int x, int y, int* w);

// Check whether a display is pending reception.
// Returns nonzero if a call to RecvDisplay will not block.
// Returns zero if a call to RecvDisplay will block.
int CNSL_PollDisplay(CNSL_Client client);

// Read the geometry of the window (from CNSLWIDTH and CNSLHEIGHT environment
// variables). Returns the results in width, height. If nothing is specified,
// the width and height are left unchanged.
// Used by clients.
void CNSL_GetGeometry(int* width, int* height);

// Set the geometry of the window (by setting CNSLWIDTh and CNSLHEIGHT
// environment variables).
// Used by servers.
void CNSL_SetGeometry(int width, int height);


#endif//CONSOLER_H

