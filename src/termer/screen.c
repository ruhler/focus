
#include <stdlib.h>

#include "screen.h"

bool eq_style(const SCREEN_Style* a, const SCREEN_Style* b)
{
    return a->reverse == b->reverse && a->bold == b->bold;
}

bool eq_attributes(const SCREEN_Attributes* a, const SCREEN_Attributes* b)
{
    return a->fgcolor == b->fgcolor
        && a->bgcolor == b->bgcolor
        && eq_style(&a->style, &b->style);
}

SCREEN_Position home()
{
    SCREEN_Position pos = {0, 0};
    return pos;
}

SCREEN_Style normal()
{
    SCREEN_Style style = { false, false};
    return style;
}

SCREEN_Attributes default_attributes()
{
    SCREEN_Attributes attr;
    attr.fgcolor = SCREEN_COLOR_WHITE;
    attr.bgcolor = SCREEN_COLOR_BLACK;
    attr.style = normal();
    return attr;
}

void nop(SCREEN_Position pos, const SCREEN_Cell* cell)
{}

SCREEN_Screen screen(int cols, int lns)
{
    SCREEN_Screen scr;
    scr.columns = cols;
    scr.lines = lns;
    scr.cursor = home();
    scr.cells = malloc(cols * lns * sizeof(SCREEN_Cell));
    scr.oldcells = malloc(cols * lns * sizeof(SCREEN_Cell));
    scr.sattrs = default_attributes();

    // initialize both cells and oldcells.
    clear_screen(&scr);
    diff(&scr, nop);
    return scr;
}

void carriage_return(SCREEN_Screen* scr)
{
    column_address(scr, 0);
}

void newline(SCREEN_Screen* scr)
{
    cursor_down(scr);
    carriage_return(scr);
}

void tab(SCREEN_Screen* scr)
{
    parm_right_cursor(scr, 8 - (scr->cursor.column % 8));
}

void column_address(SCREEN_Screen* scr, int col)
{
    SCREEN_Position pos;
    pos.line = scr->cursor.line;
    pos.column = col;
    cursor_address(scr, pos);
}

void row_address(SCREEN_Screen* scr, int row)
{
    SCREEN_Position pos;
    pos.column = scr->cursor.column;
    pos.line = row;
    cursor_address(scr, pos);
}

void cursor_address(SCREEN_Screen* scr, SCREEN_Position pos)
{
    scr->cursor = pos;
}

void cursor_down(SCREEN_Screen* scr)
{
    if (scr->cursor.line == scr->lines - 1) {
        scroll_forward(scr);
    } else {
        parm_down_cursor(scr, 1);
    }
}

void cursor_home(SCREEN_Screen* scr)
{
    cursor_address(scr, home());
}

void cursor_left(SCREEN_Screen* scr)
{
    parm_left_cursor(scr, 1);
}

void cursor_right(SCREEN_Screen* scr)
{
    parm_right_cursor(scr, 1);
}

void cursor_to_ll(SCREEN_Screen* scr)
{
    SCREEN_Position pos;
    pos.column = 0;
    pos.line = scr->lines-1;
    cursor_address(scr, pos);
}

void cursor_up(SCREEN_Screen* scr)
{
    parm_up_cursor(scr, 1);
}

void parm_left_cursor(SCREEN_Screen* scr, int n)
{
    scr->cursor.column -= n;
}

void parm_right_cursor(SCREEN_Screen* scr, int n)
{
    scr->cursor.column += n;
}

void parm_up_cursor(SCREEN_Screen* scr, int n)
{
    scr->cursor.line -= n;
}

void parm_down_cursor(SCREEN_Screen* scr, int n)
{
    scr->cursor.line += n;
}

SCREEN_Cell blank(const SCREEN_Screen* scr)
{
    SCREEN_Cell cell;
    cell.character = ' ';
    cell.cattrs = scr->sattrs;
    return cell;
}

SCREEN_Cell getcell(SCREEN_Screen* scr, int x, int y)
{
    return scr->cells[y*scr->columns + x];
}

void setcell(SCREEN_Screen* scr, int x, int y, SCREEN_Cell cell)
{
    scr->cells[y*scr->columns + x] = cell;
}

void clear_screen(SCREEN_Screen* scr)
{
    int x, y;
    for (y = 0; y < scr->lines; y++) {
        for (x = 0; x < scr->columns; x++) {
            setcell(scr, x, y, blank(scr));
        }
    }
    scr->cursor = home();
}

void clr_bol(SCREEN_Screen* scr)
{
    int x;
    for (x = 0; x <= scr->cursor.column; x++) {
        setcell(scr, x, scr->cursor.line, blank(scr));
    }
}

void clr_eol(SCREEN_Screen* scr)
{
    int x;
    for (x = scr->cursor.column; x < scr->columns; x++) {
        setcell(scr, x, scr->cursor.line, blank(scr));
    }
}

void clr_eos(SCREEN_Screen* scr)
{
    clr_eol(scr);
    int x, y;
    for (y = scr->cursor.line+1; y < scr->lines; y++) {
        for (x = 0; x < scr->columns; x++) {
            setcell(scr, x, y, blank(scr));
        }
    }
}

void enter_bold_mode(SCREEN_Screen* scr)
{
    scr->sattrs.style.bold = true;
}

void enter_reverse_mode(SCREEN_Screen* scr)
{
    scr->sattrs.style.reverse = true;
}

void exit_attribute_mode(SCREEN_Screen* scr)
{
    scr->sattrs = default_attributes();
}

void exit_bold_mode(SCREEN_Screen* scr)
{
    scr->sattrs.style.bold = false;
}

void set_foreground(SCREEN_Screen* scr, SCREEN_Color c)
{
    scr->sattrs.fgcolor = c;
}

void set_background(SCREEN_Screen* scr, SCREEN_Color c)
{
    scr->sattrs.bgcolor = c;
}

void delete_character(SCREEN_Screen* scr)
{
    parm_dch(scr, 1);
}

void parm_dch(SCREEN_Screen* scr, int n)
{
    int x;
    for (x = scr->cursor.column; x < scr->columns; x++) {
        SCREEN_Cell ncell = x+n >= scr->columns ? blank(scr) : getcell(scr, x+n, scr->cursor.line);
        setcell(scr, x, scr->cursor.line, ncell);
    }
}

void delete_line(SCREEN_Screen* scr)
{
    parm_delete_line(scr, 1);
}

void parm_delete_line(SCREEN_Screen* scr, int n)
{
    int x, y;
    for (y = scr->cursor.line; y < scr->lines; y++) {
        for (x = 0; x < scr->columns; x++) {
            SCREEN_Cell ncell = y+n >= scr->lines ? blank(scr) : getcell(scr, x, y+n);
            setcell(scr, x, y, ncell);
        }
    }
}

void erase_chars(SCREEN_Screen* scr, int n)
{
    int x;
    for (x = scr->cursor.column; x <= scr->cursor.column + n; x++) {
        setcell(scr, x, scr->cursor.line, blank(scr));
    }
}

void insert_character(SCREEN_Screen* scr)
{
    parm_ich(scr, 1);
}

void parm_ich(SCREEN_Screen* scr, int n)
{
    int x;
    for (x = scr->columns-1; x >= scr->cursor.column + n; x--) {
        SCREEN_Cell ncell = x-n >= 0 ? getcell(scr, x-n, scr->cursor.line) : blank(scr);
        setcell(scr, x, scr->cursor.line, ncell);
    }
    for (x = scr->cursor.column; x < scr->cursor.column+n; x++) {
        setcell(scr, x, scr->cursor.line, blank(scr));
    }
}

void insert_line(SCREEN_Screen* scr)
{
    parm_insert_line(scr, 1);
}

void parm_insert_line(SCREEN_Screen* scr, int n)
{
    int x, y;
    for (y = scr->lines-1; y >= scr->cursor.line+n; y--) {
        for (x = 0; x < scr->columns; x++) {
            SCREEN_Cell ncell = y-n >= 0 ? getcell(scr, x, y-n) : blank(scr);
            setcell(scr, x, y, ncell);
        }
    }

    for (y = scr->cursor.line; y < scr->cursor.line+n; y++) {
        for (x = 0; x < scr->columns; x++) {
            setcell(scr, x, y, blank(scr));
        }
    }
}

void scroll_forward(SCREEN_Screen* scr)
{
    parm_index(scr, 1);
}

void parm_index(SCREEN_Screen* scr, int n)
{
    int x, y;
    for (y = 0; y < scr->lines; y++) {
        for (x = 0; x < scr->columns; x++) {
            SCREEN_Cell ncell = y+n < scr->lines ? getcell(scr, x, y+n) : blank(scr);
            setcell(scr, x, y, ncell);
        }
    }
}

void scroll_reverse(SCREEN_Screen* scr)
{
    parm_rindex(scr, 1);
}

void parm_rindex(SCREEN_Screen* scr, int n)
{
    int x, y;
    for (y = scr->lines-1; y >= 0; y--) {
        for (x = 0; x < scr->columns; x++) {
            SCREEN_Cell ncell = y-n >= 0 ? getcell(scr, x, y-n) : blank(scr);
            setcell(scr, x, y, ncell);
        }
    }
}

void put_char(SCREEN_Screen* scr, char c)
{
    if (scr->cursor.line == scr->lines)
    {
        scroll_forward(scr);
        cursor_up(scr);
        put_char(scr, c);
    } else {
        SCREEN_Cell ncell = blank(scr);
        ncell.character = c;
        setcell(scr, scr->cursor.column, scr->cursor.line, ncell);
        if (scr->cursor.column+1 >= scr->columns) {
            scr->cursor.column = 0;
            scr->cursor.line++;
        } else {
            scr->cursor.column++;
        }
    }
}

SCREEN_Cell cellat(SCREEN_Screen* scr, SCREEN_Position pos)
{
    return getcell(scr, pos.column, pos.line);
}

SCREEN_Position cursor(SCREEN_Screen* scr)
{
    return scr->cursor;
}

bool eq_cell(const SCREEN_Cell* a, const SCREEN_Cell* b)
{
    return a->character == b->character && eq_attributes(&a->cattrs, &b->cattrs);
}

bool eq_position(const SCREEN_Position* a, const SCREEN_Position* b)
{
    return a->column == b->column && a->line == b->line;
}

bool eq_screen(const SCREEN_Screen* a, const SCREEN_Screen* b)
{
    if (a->columns != b->columns
            || a->lines != b->lines
            || !eq_position(&a->cursor, &b->cursor)
            || !eq_attributes(&a->sattrs, &b->sattrs))
    {
        return false;
    }

    int x, y;
    for (y = 0; y < a->lines; y++) {
        for (x = 0; x < a->columns; x++) {
            SCREEN_Cell* anew = &a->cells[y*a->columns + x];
            SCREEN_Cell* aold = &a->oldcells[y*a->columns + x];
            SCREEN_Cell* bnew = &b->cells[y*a->columns + x];
            SCREEN_Cell* bold = &b->oldcells[y*a->columns + x];
            if (!eq_cell(anew, bnew) || !eq_cell(aold, bold)) {
                return false;
            }
        }
    }

    return true;
}

void diff(SCREEN_Screen* scr, DiffFunction df)
{
    int x, y;
    for (y = 0; y < scr->lines; y++) {
        for (x = 0; x < scr->columns; x++) {
            SCREEN_Cell* new = &scr->cells[y*scr->columns + x];
            SCREEN_Cell* old = &scr->oldcells[y*scr->columns + x];
            if (!eq_cell(old, new)) {
                SCREEN_Position pos;
                pos.line = y;
                pos.column = x;
                df(pos, new);
                scr->oldcells[y*scr->columns+x] = *new;
            }
        }
    }
}

SCREEN_Position mkpos(int col, int line)
{
    SCREEN_Position pos = {col, line};
    return pos;
}


