
// Copyright (C) 2011 Richard Uhler <ruhler@member.fsf.org>
//
// This file is part of Focus.
//
// Focus is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.
//
// Focus is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with Focus.  If not, see <http://www.gnu.org/licenses/>.

#ifndef CONSOLER_H
#define CONSOLER_H

#include <stdbool.h>
#include <stdint.h>

#include "consoler_keysym.h"

/// CNSL_Version - get consoler version
///
/// Return a string describing the version of the library.
const char* CNSL_Version();

typedef struct {
    int type;
    int value;
} CNSL_Event;

/// CNSL_MakeKeypress - construct a keypress event
///
/// Construct a keypress event with the given keysym.
CNSL_Event CNSL_MakeKeypress(CNSL_Keysym sym);

/// CNSL_MakeKeyrelease - construct a keyrelease event
///
/// Construct a keyrelease event with the given keysym.
CNSL_Event CNSL_MakeKeyrelease(CNSL_Keysym sym);

/// CNSL_MakeQuit - construct a quit event.
///
/// Construct a quit event.
CNSL_Event CNSL_MakeQuit();

/// CNSL_IsKeypress - deconstruct a key press event
///
/// Return true if 'event' is a keypress event, and output the keysym
/// associated with the keypress event to the 'sym' parameter.  Otherwise
/// return false and 'sym' is untouched.
bool CNSL_IsKeypress(CNSL_Event event, CNSL_Keysym* sym);

/// CNSL_IsKeyrelease - deconstruct a key release event
///
/// Return true if 'event' is a keyrelease event, and output the keysym
/// associated with the keyrelease event to the 'sym' parameter.  Otherwise
/// return false and 'sym' is untouched.
bool CNSL_IsKeyrelease(CNSL_Event event, CNSL_Keysym* sym);

/// CNSL_IsQuit - deconstruct a quit event
///
/// Return true if 'event' is a quit event, false otherwise.
bool CNSL_IsQuit(CNSL_Event event);

typedef uint32_t CNSL_Color;

/// CNSL_GetRed8 - get red component of a color
///
/// Return the red component of the color 'c'. The value returned is in the
/// range 0 to 255 inclusive.
uint8_t CNSL_GetRed8(CNSL_Color c);

/// CNSL_GetBlue8 - get blue component of a color
///
/// Return the blue component of the color 'c'. The value returned is in the
/// range 0 to 255 inclusive.
uint8_t CNSL_GetBlue8(CNSL_Color c);

/// CNSL_GetGreen8 - get green component of a color
///
/// Return the green component of the color 'c'. The value returned is in the
/// range 0 to 255 inclusive.
uint8_t CNSL_GetGreen8(CNSL_Color c);

/// CNSL_GetRGB8 - pack a color
///
/// Get the component values of the color 'c' all together in a single word.
/// Bits 23:16 contain the red component, 15:8 contain the green component,
/// and 7:0 contain the blue component of the color.
///
/// This function is currently the identify function, but will be useful if
/// later on the underlying representation for +CNSL_Color+ changes.
uint32_t CNSL_GetRGB8(CNSL_Color c);

/// CNSL_MakeColor - construct a color
///
/// Construct a color with the given red, green, and blue components.  Each
/// component should be in the range 0 to 255 inclusive.
CNSL_Color CNSL_MakeColor(uint8_t r, uint8_t g, uint8_t b);


typedef struct {
    unsigned int width;
    unsigned int height;
    CNSL_Color* pixels;
} CNSL_Display;


/// CNSL_AllocDisplay - allocate a display
///
/// Allocate a display of the given dimensions. More specifically, this
/// allocates memory for the pixel data of the display.
///
/// The display should be freed with a call to +CNSL_FreeDisplay+ when it is no
/// longer needed.
///
/// If there is in error allocating the display, the returned display's pixels
/// field will be NULL.
CNSL_Display CNSL_AllocDisplay(unsigned int width, unsigned int height);

/// CNSL_FreeDisplay - free an allocated display
///
/// Free memory associated with the pixel data for the given display.
void CNSL_FreeDisplay(CNSL_Display display);

/// CNSL_GetPixel - read a pixel from a display
///
/// Get the color of the pixel at the given location in the display.
///
/// It is undefined what happens if 'x' and 'y' are outside the dimensions of
/// the display.
CNSL_Color CNSL_GetPixel(CNSL_Display display, unsigned int x, unsigned int y);

/// CNSL_SetPixel - write a pixel to a display
///
/// Set the color of the pixel at the given location in the display.
///
/// It is undefined what happens if 'x' and 'y' are outside the dimensions of
/// the display.
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

/// CNSL_LaunchClient - launch a consoler client
///
/// Launch a consoler client specified by the given path and pass it the
/// given arguments. This forks a new processes to exec the client, sets
/// up a communication channel with the client using pipes, and returns a
/// reference to the client.
///
/// If there is an error in launching the client, both fields of the
/// returned client will be set to -1.
CNSL_Client CNSL_LaunchClient(const char* path, char* const args[]);

/// CNSL_CloseClient - close a connection to a client
///
/// Close a connection to a client.
void CNSL_CloseClient(CNSL_Client client);

/// CNSL_RecvEvent - get an event from the server
///
/// Get the next event from the console server. This function blocks until
/// the next event is available.  Returns a quit event if server
/// connection has closed.
CNSL_Event CNSL_RecvEvent(CNSL_Console console);

/// CNSL_SendEvent - send an event to a client
///
/// Send an event to a client.
///
/// Returns true if the event was sent successfully, false otherwise.
bool CNSL_SendEvent(CNSL_Client client, CNSL_Event event);

/// CNSL_SendDisplay - send a display to the server
///
/// Send a display to a server. Pixels are sent from the rectangular
/// box with given 'width' and 'height' and upper left corner at
/// coordinates ('srcx', 'srcy'). The pixels will be written to the
/// destination coordinates ('dstx', 'dsty') of a master display the
/// server keeps track of for the client.
void CNSL_SendDisplay(CNSL_Console console, CNSL_Display display,
        unsigned int srcx, unsigned int srcy,
        unsigned int dstx, unsigned int dsty,
        unsigned int width, unsigned int height);

/// CNSL_RecvDisplay - receive a display from a client
///
/// Receive a display from a client by copying it to the given display.
/// The location of the update will be as specified by the client when it
/// called SendDisplay. Pixels not in range of 'display' will be
/// ignored.
///
/// The parameters 'dstx', 'dsty', 'width', and 'height' are set to the updated
/// region of the display. They may be NULL.
///
/// Returns true if a display was recieved, false on end of file or other
/// error.
bool CNSL_RecvDisplay(CNSL_Client client, CNSL_Display display,
        unsigned int* dstx, unsigned int* dsty,
        unsigned int* width, unsigned int* height);

/// CNSL_GetGeometry - indicate window geometry
///
/// Set the environment variables +CNSLWIDTH+ and +CNSLHEIGHT+ to the given
/// width and height. This can be called by a server before the server
/// launches a client to propagate the screen dimensions to the client.
void CNSL_GetGeometry(int* width, int* height);

/// CNSL_SetGeometry - get window geometry
///
/// Read the environment variables +CNSLWIDTH+ and +CNSLHEIGHT+ into 'width' and
/// 'height'. If those environment variables are not set, 'width' and 'height'
/// are unchanged. This can be used by the client to discover the screen
/// dimensions when it first starts.
void CNSL_SetGeometry(int width, int height);


#endif//CONSOLER_H

