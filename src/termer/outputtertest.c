
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

#include <stdio.h>

#include "outputter.h"

const char* runstringbuf = NULL;

char runstringgetf()
{
    char c = *runstringbuf;
    runstringbuf++;
    return c;
}

// update the given screen by running outputter on the given string.
void runstring(SCREEN_Screen* scr, const char* str)
{
    runstringbuf = str;
    outputter(scr, '\0', runstringgetf);
}

void update(bool* result, bool x)
{
    *result = *result && x;
}

bool tscreq(const char* name, SCREEN_Screen* a, SCREEN_Screen* b)
{
    if (!eq_screen(a, b)) {
        printf("%s:\n", name);

        if (!eq_position(&a->cursor, &b->cursor)) {
            printf("\tecursor = %i,%i\n", a->cursor.column, a->cursor.line);
            printf("\tgcursor = %i,%i\n", b->cursor.column, b->cursor.line);
        }

        if (!eq_attributes(&a->sattrs, &b->sattrs)) {
            printf("\teattr = fg %i, bg %i, b %i, r %i\n",
                    a->sattrs.fgcolor, a->sattrs.bgcolor,
                    a->sattrs.style.bold, a->sattrs.style.reverse);
            printf("\tgattr = fg %i, bg %i, b %i, r %i\n",
                    b->sattrs.fgcolor, b->sattrs.bgcolor,
                    b->sattrs.style.bold, b->sattrs.style.reverse);
        }

        int x, y;
        for (y = 0; y < a->lines; y++) {
            for (x = 0; x < a->columns; x++) {
                SCREEN_Cell acell = cellat(a, mkpos(x, y));
                SCREEN_Cell bcell = cellat(b, mkpos(x, y));
                if (!eq_cell(&acell, &bcell)) {
                    printf("\tecell %i,%i = %c(%i), fg %i, bg %i, b %i, r %i\n",
                            x, y, acell.character, acell.character,
                            acell.cattrs.fgcolor, acell.cattrs.bgcolor,
                            acell.cattrs.style.bold, acell.cattrs.style.reverse);
                    printf("\tgcell %i,%i = %c(%i) fg %i bg %i, b %i, r %i\n",
                            x, y, bcell.character, bcell.character,
                            bcell.cattrs.fgcolor, bcell.cattrs.bgcolor,
                            bcell.cattrs.style.bold, bcell.cattrs.style.reverse);
                }
            }
        }

        return false;
    }
    return true;
}

void test(bool* result)
{
    SCREEN_Screen wnt;
    SCREEN_Screen got;

    wnt = screen(6, 4);
    got = screen(6, 4);
    runstring(&got, "a");
    put_char(&wnt, 'a');
    update(result, tscreq("simple", &wnt, &got));

    wnt = screen(6, 4);
    got = screen(6, 4);
    runstring(&got, "abc");
    put_char(&wnt, 'a');
    put_char(&wnt, 'b');
    put_char(&wnt, 'c');
    update(result, tscreq("multi", &wnt, &got));

    wnt = screen(6, 4);
    got = screen(6, 4);
    runstring(&got, "ab\n\rc");
    put_char(&wnt, 'a');
    put_char(&wnt, 'b');
    newline(&wnt);
    put_char(&wnt, 'c');
    update(result, tscreq("newline", &wnt, &got));

    wnt = screen(6, 4);
    got = screen(6, 4);
    runstring(&got, "ab\bc");
    put_char(&wnt, 'a');
    put_char(&wnt, 'b');
    cursor_left(&wnt);
    put_char(&wnt, 'c');
    update(result, tscreq("backspace", &wnt, &got));

    wnt = screen(6, 4);
    got = screen(6, 4);
    runstring(&got, "ab\e[mc");
    put_char(&wnt, 'a');
    put_char(&wnt, 'b');
    exit_attribute_mode(&wnt);
    put_char(&wnt, 'c');
    update(result, tscreq("ESC[m", &wnt, &got));

    wnt = screen(6, 4);
    got = screen(6, 4);
    runstring(&got, "ab\e[0mc");
    put_char(&wnt, 'a');
    put_char(&wnt, 'b');
    exit_attribute_mode(&wnt);
    put_char(&wnt, 'c');
    update(result, tscreq("ESC[0m", &wnt, &got));

    wnt = screen(6, 4);
    got = screen(6, 4);
    runstring(&got, "a\e[1mb\e[0;10mc");
    put_char(&wnt, 'a');
    enter_bold_mode(&wnt);
    put_char(&wnt, 'b');
    exit_attribute_mode(&wnt);
    put_char(&wnt, 'c');
    update(result, tscreq("sgr: ESC[0;10m", &wnt, &got));

    wnt = screen(6, 4);
    got = screen(6, 4);
    runstring(&got, "a\e[1mb\e[22mc");
    put_char(&wnt, 'a');
    enter_bold_mode(&wnt);
    put_char(&wnt, 'b');
    exit_bold_mode(&wnt);
    put_char(&wnt, 'c');
    update(result, tscreq("boldoff: ESC[22m", &wnt, &got));

    wnt = screen(6, 4);
    got = screen(6, 4);
    runstring(&got, "\e[4;3H");
    cursor_address(&wnt, mkpos(2,3));
    update(result, tscreq("cup: ESC[4;3H", &wnt, &got));

    wnt = screen(6, 4);
    got = screen(6, 4);
    runstring(&got, "\e[4C");
    parm_right_cursor(&wnt, 4);
    update(result, tscreq("cuf: ESC[4C", &wnt, &got));

    wnt = screen(24, 4);
    got = screen(24, 4);
    runstring(&got, "a\tb");
    put_char(&wnt, 'a');
    tab(&wnt);
    put_char(&wnt, 'b');
    update(result, tscreq("tab", &wnt, &got));

    wnt = screen(6, 4);
    got = screen(6, 4);
    runstring(&got, "a\e[1;34mb");
    put_char(&wnt, 'a');
    enter_bold_mode(&wnt);
    set_foreground(&wnt, SCREEN_COLOR_BLUE);
    put_char(&wnt, 'b');
    update(result, tscreq("ESC[1;34m", &wnt, &got));

    wnt = screen(6, 4);
    got = screen(6, 4);
    runstring(&got, "\e[42ma\e[49mb");
    set_background(&wnt, SCREEN_COLOR_GREEN);
    put_char(&wnt, 'a');
    set_background(&wnt, SCREEN_COLOR_BLACK);
    put_char(&wnt, 'b');
    update(result, tscreq("default background color", &wnt, &got));

    wnt = screen(6, 4);
    got = screen(6, 4);
    runstring(&got, "\xE2\x80\x98m");
    put_char(&wnt, 0x2018);
    put_char(&wnt, 'm');
    update(result, tscreq("unicode", &wnt, &got));

    wnt = screen(6, 4);
    got = screen(6, 4);
    runstring(&got, "\e[3;3Ha\eM");
    cursor_address(&wnt, mkpos(2, 2));
    put_char(&wnt, 'a');
    scroll_reverse(&wnt);
    update(result, tscreq("ESC M", &wnt, &got));
}

int main()
{
    bool result = true;
    test(&result);
    if (result) {
        printf("all tests passed.\n");
        return 0;
    } else {
        printf("there were failing tests.\n");
        return 1;
    }
}
