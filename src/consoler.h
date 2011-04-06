
#ifndef CONSOLER_H
#define CONSOLER_H

#include "consoler_keysym.h"

// A color. Represented as 0x00RRGGBB.
// One byte for eac red, green, blue component.
typedef unsigned int CNSL_Color;

// Return the components of the color.
// Returned value is in interval [0, 255]
int CNSL_getred(CNSL_Color c);
int CNSL_getblue(CNSL_Color c);
int CNSL_getgreen(CNSL_Color c);

// Create a color from the given components.
// Component values should be in the interval [0, 255].
CNSL_Color CNSL_rgb8(int r, int g, int b);


typedef struct {
    unsigned int width;
    unsigned int height;
    CNSL_Color* pixels;
} CNSL_Display_;

typedef CNSL_Display_* CNSL_Display;

// Allocate a pixel buffer
// Returns NULL if there was some problem allocating the buffer.
// The buffer should be freed when you're done using it.
CNSL_Display CNSL_allocdisplay(unsigned int width, unsigned int height);

// Free an allocated display.
void CNSL_display_free(CNSL_Display display);

// Get the color of the pixel at the given location in the display.
CNSL_Color CNSL_getpixel(CNSL_Display display, unsigned int x, unsigned int y);

// Set the color of the pixel at the given location in the display.
void CNSL_setpixel(CNSL_Display display, unsigned int x, unsigned int y, CNSL_Color color);


// Event types
#define CNSLE_KEYPRESS 0
#define CNSLE_KEYRELEASE 1

typedef struct {
    int type;
    int value;
} CNSL_Event;

// Test whether an event is a keypress, and if it is, read the key code.
// Returns nonzero if the event is a keypress event.
int CNSL_iskeypress(const CNSL_Event* e, int* code);

// Test whether an event is a keyrelease, and if it is, read the key code.
// Returns nonzero if the event is a keypress event.
int CNSL_iskeyrelease(const CNSL_Event* e, int* code);


typedef struct {
    FILE* fin;
    FILE* fout;
} CNSL_Console;

extern CNSL_Console stdcon;

typedef struct {
    FILE* fin;
    FILE* fout;
} CNSL_Client_;

typedef CNSL_Client_* CNSL_Client;

// Launch a client program, returning a handle to the client.
// Returns NULL on failure
// Arguments are same as for exec.
CNSL_Client CNSL_launch_client(const char* path, char* const args[]);
void CNSL_close_client(CNSL_Client client);

// Get the next event from the console.
// Returns zero if no more events, -1 on error, nonzero on success.
int CNSL_event_recv(CNSL_Console console, CNSL_Event* event);

// Send an event to a client
// Returns -1 on error, nonzero on success.
// 0 on no more client?
int CNSL_event_send(CNSL_Client client, const CNSL_Event* event);

// Send a display from the client to the server.
// srcx and srcy specify where in display to start taking pixels from.
// width and height specify the dimensions of the pixel block to send.
// dstx and dsty are the destination for the block on the Console's display
// for this client.
int CNSL_display_send(CNSL_Console console, CNSL_Display display,
        unsigned int srcx, unsigned int srcy,
        unsigned int dstx, unsigned int dsty,
        unsigned int width, unsigned int height);

// Get a screen update from the client.
// This is a blocking call.
// Calls function f for each pixel to be updated. Returns the updated
// rectangle via x, y, w, and h.
// Returns nonzero on success, zero on end of file, -1 on error.
typedef void (CNSL_DRFunction)(void* ud, int x, int y, CNSL_Color c);
int CNSL_display_recv(CNSL_Client client,
        unsigned int* dstx, unsigned int* dsty,
        unsigned int* width, unsigned int* height,
        CNSL_DRFunction f, void* ud);


#endif//CONSOLER_H

