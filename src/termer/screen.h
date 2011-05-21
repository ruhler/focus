
#ifndef SCREEN_H
#define SCREEN_H

#include "bool.h"

typedef int SCREEN_Color;

#define SCREEN_COLOR_BLACK 0
#define SCREEN_COLOR_RED 1
#define SCREEN_COLOR_GREEN 2
#define SCREEN_COLOR_YELLOW 3
#define SCREEN_COLOR_BLUE 4
#define SCREEN_COLOR_MAGENTA 5
#define SCREEN_COLOR_CYAN 6
#define SCREEN_COLOR_WHITE 7

typedef struct {
    bool reverse;
    bool bold;
} SCREEN_Style;

typedef struct {
    SCREEN_Color fgcolor;
    SCREEN_Color bgcolor;
    SCREEN_Style style;
} SCREEN_Attributes;

typedef struct {
    char character;
    SCREEN_Attributes cattrs;
} SCREEN_Cell;

bool celleq(const SCREEN_Cell* a, const SCREEN_Cell* b);

typedef struct {
    int column;
    int line;
} SCREEN_Position;

SCREEN_Position mkpos(int col, int line);

typedef struct {
    int columns;
    int lines;
    SCREEN_Position cursor;
    SCREEN_Cell* cells;
    SCREEN_Cell* oldcells;
    SCREEN_Attributes sattrs;
} SCREEN_Screen;


// TODO: this leaks memory. So don't call this a lot, or write a function
// to clean up the leaked memory and remember to call that.
SCREEN_Screen screen(int cols, int lns);

void carriage_return(SCREEN_Screen* scr);
void newline(SCREEN_Screen* scr);
void tab(SCREEN_Screen* scr);
void column_address(SCREEN_Screen* scr, int col);
void row_address(SCREEN_Screen* scr, int row);
void cursor_address(SCREEN_Screen* scr, SCREEN_Position pos);
void cursor_down(SCREEN_Screen* scr);
void cursor_home(SCREEN_Screen* scr);
void cursor_left(SCREEN_Screen* scr);
void cursor_right(SCREEN_Screen* scr);
void cursor_to_ll(SCREEN_Screen* scr);
void cursor_up(SCREEN_Screen* scr);
void parm_left_cursor(SCREEN_Screen* scr, int n);
void parm_right_cursor(SCREEN_Screen* scr, int n);
void parm_up_cursor(SCREEN_Screen* scr, int n);
void parm_down_cursor(SCREEN_Screen* scr, int n);
void clear_screen(SCREEN_Screen* scr);
void clr_bol(SCREEN_Screen* scr);
void clr_eol(SCREEN_Screen* scr);
void clr_eos(SCREEN_Screen* scr);
void enter_bold_mode(SCREEN_Screen* scr);
void enter_reverse_mode(SCREEN_Screen* scr);
void exit_attribute_mode(SCREEN_Screen* scr);
void exit_bold_mode(SCREEN_Screen* scr);
void set_foreground(SCREEN_Screen* scr, SCREEN_Color c);
void set_background(SCREEN_Screen* scr, SCREEN_Color c);
void delete_character(SCREEN_Screen* scr);
void parm_dch(SCREEN_Screen* scr, int n);
void delete_line(SCREEN_Screen* scr);
void parm_delete_line(SCREEN_Screen* scr, int n);
void erase_chars(SCREEN_Screen* scr, int n);
void insert_character(SCREEN_Screen* scr);
void parm_ich(SCREEN_Screen* scr, int n);
void insert_line(SCREEN_Screen* scr);
void parm_insert_line(SCREEN_Screen* scr, int n);
void scroll_forward(SCREEN_Screen* scr);
void parm_index(SCREEN_Screen* scr, int n);
void scroll_reverse(SCREEN_Screen* scr);
void parm_rindex(SCREEN_Screen* scr, int n);
void put_char(SCREEN_Screen* scr, char c);

SCREEN_Cell cellat(SCREEN_Screen* scr, SCREEN_Position pos);
SCREEN_Position cursor(SCREEN_Screen* scr);

// diff: calls the given function for each cell which has changed.
typedef void (*DiffFunction)(SCREEN_Position, const SCREEN_Cell*);
void diff(SCREEN_Screen* scr, DiffFunction df);

#endif//SCREEN_H

