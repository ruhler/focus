
#ifndef CCL_H
#define CCL_H

// Console Client Library

// A color. Represented as 0x00RRGGBB.
// One byte for each red, green, blue component.
typedef unsigned int Color;

// Clear the display.
void ccl_clear();

// Set the color of the given pixel on the display.
void ccl_pixel(int x, int y, Color c);

// Fill the given rectangle with the given color.
void ccl_fill(int x, int y, int w, int h, Color c);

// Return a color with given components of red, green, and blue.
// The component values are from 0 to 255.
Color ccl_rgb8(int r, int g, int b);

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

