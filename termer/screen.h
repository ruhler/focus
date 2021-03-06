
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

#ifndef SCREEN_H
#define SCREEN_H

#include <stdbool.h>
#include <wchar.h>


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

bool eq_style(const SCREEN_Style* a, const SCREEN_Style* b);

typedef struct {
    SCREEN_Color fgcolor;
    SCREEN_Color bgcolor;
    SCREEN_Style style;
} SCREEN_Attributes;

bool eq_attributes(const SCREEN_Attributes* a, const SCREEN_Attributes* b);

typedef struct {
    wchar_t character;
    SCREEN_Attributes cattrs;
} SCREEN_Cell;

bool eq_cell(const SCREEN_Cell* a, const SCREEN_Cell* b);

typedef struct {
    int column;
    int line;
} SCREEN_Position;

bool eq_position(const SCREEN_Position* a, const SCREEN_Position* b);

SCREEN_Position mkpos(int col, int line);

typedef struct {
    int columns;
    int lines;
    SCREEN_Position cursor;
    SCREEN_Cell* cells;
    SCREEN_Cell* oldcells;
    SCREEN_Attributes sattrs;
} SCREEN_Screen;

bool eq_screen(const SCREEN_Screen* a, const SCREEN_Screen* b);


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
void put_char(SCREEN_Screen* scr, wchar_t c);

SCREEN_Cell cellat(SCREEN_Screen* scr, SCREEN_Position pos);
SCREEN_Position cursor(SCREEN_Screen* scr);

// resize the screen to the given number of columns and lines.
void SCREEN_Resize(SCREEN_Screen* scr, int columns, int lines);

// diff: calls the given function for each cell which has changed.
typedef void (*DiffFunction)(SCREEN_Position, const SCREEN_Cell*);
void diff(SCREEN_Screen* scr, DiffFunction df);

#endif//SCREEN_H

