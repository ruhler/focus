
#ifndef CTERMER_H
#define CTERMER_H

// Initialize CTERMER 
//  - initializes CNSL, freetype,
//  - forks terminal client process /bin/bash
// Returns 0 on success, nonzero on error.
int ctermer_Init();
void ctermer_DeInit();

// Make it so anyone blocked in ctermer_EventGet will get a QuitEvent.
void ctermer_Quit();

// Get the next event.
// Stores the event in a static place accessible via the EventType and
// EventValue functions.
void ctermer_EventGet();

// Get the type of the most recently gotten event.
int ctermer_EventType();

// Get the value of the most recently gotten event.
int ctermer_EventValue();

// Send the given character to the terminal client
void ctermer_ToTermClient(char c);

// Get a string of the next characters from the terminal client.
// The string returned is a pointer to a static buffer. It will remain valid
// until this function is called again, in which case the buffer will be
// updated with the new string.
char* ctermer_FromTermClient();

// Cell styles
#define CTERMER_STYLE_NORMAL 0
#define CTERMER_STYLE_BOLD 1

// Cell colors
#define CTERMER_COLOR_BLACK 0
#define CTERMER_COLOR_RED 1
#define CTERMER_COLOR_GREEN 2
#define CTERMER_COLOR_YELLOW 3
#define CTERMER_COLOR_BLUE 4
#define CTERMER_COLOR_MAGENTA 5
#define CTERMER_COLOR_CYAN 6
#define CTERMER_COLOR_WHITE 7

// Draw the cell on the display
//  col - cell column (0 indexed)
//  rol - cell row (0 indexed)
//  c - character of cell
//  style - CTERMER_STYLE_* - normal or bold
//  fgcolor - foreground color: CTERMER_COLOR_*
void ctermer_DrawCell(int col, int row, char c, int style, int fgcolor, int bgcolor);

// Send the current display to the consol server.
void ctermer_ShowDisplay();

#endif//CTERMER_H

