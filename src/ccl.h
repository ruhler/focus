
#ifndef CCL_H
#define CCL_H

// Console Client Library

#include "ccl_keysym.h"

// A color. Represented as 0x00RRGGBB.
// One byte for each red, green, blue component.
typedef unsigned int Color;

typedef struct {
    unsigned int width;
    unsigned int height;
    Color* pixels;
} Buffer_;

typedef Buffer_* Buffer;

// Allocate a pixel buffer.
// Returns NULL if there was some problem allocating the buffer.
// The buffer should be freed when you're done using it.
Buffer ccl_alloc_buffer(unsigned int width, unsigned int height);

// Free an allocated pixel buffer.
void ccl_free_buffer(Buffer buffer);

// Copy bits from the given buffer (allocated by alloc_buffer) to the display.
void ccl_blit(Buffer src,
        unsigned int srcx, unsigned int srcy,
        unsigned int dstx, unsigned int dsty,
        unsigned int width, unsigned int height);

// Read the color at the given pixel of the given buffer.
Color ccl_getpixel(Buffer buffer, unsigned int x, unsigned int y);

// Write the color at the given pixel of the given buffer.
void ccl_setpixel(Buffer buffer, unsigned int x, unsigned int y, Color c);

// Return a color with given components of red, green, and blue.
// The component values are from 0 to 255.
Color ccl_rgb8(int r, int g, int b);

// Extract component values from a color.
// The values returned are 8 bit.
int ccl_redof(Color c);
int ccl_greenof(Color c);
int ccl_blueof(Color c);

// Event types
#define EVENT_KEYPRESS 0
#define EVENT_KEYRELEASE 1

typedef struct {
    int type;
    int value;
} Event;

// Read the next event.
void ccl_event(Event* event);

// Test whether an event is a keypress, and if it is, read the key code.
// Returns nonzero if the event is a keypress event.
int ccl_keypress(const Event* e, int* code);

// Test whether an event is a keyrelease, and if it is, read the key code.
// Returns nonzero if the event is a keyrelease event.
int ccl_keyrelease(const Event* e, int* code);


#endif//CCL_H

