
#include <stdio.h>
#include "screen.h"

bool tcelleq(const char* name, SCREEN_Cell a, SCREEN_Cell b)
{
    if (!eq_cell(&a, &b)) {
        printf("%s: expected: ???, got: ???\n", name);
        return false;
    }
    return true;
}

bool tposeq(const char* name, SCREEN_Position a, SCREEN_Position b)
{
    if (!eq_position(&a, &b)) {
        printf("%s: expected: (%i, %i), got: (%i, %i)\n",
                name, a.column, a.line, b.column, b.line);
        return false;
    }

    return true;
}

void update(bool* result, bool x)
{
    *result = *result && x;
}

void put_chars(SCREEN_Screen* scr, const char* str)
{
    while (*str) {
        put_char(scr, *str);
        str++;
    }
}

void test(bool* result)
{
    SCREEN_Screen scr;

    scr = screen(80, 25);
    update(result, tposeq("initial cursor", mkpos(0, 0), cursor(&scr)));

    scr = screen(80, 25);
    cursor_address(&scr, mkpos(5, 10));
    update(result, tposeq("cursor_address", mkpos(5, 10), cursor(&scr)));

    scr = screen(80, 25);
    cursor_address(&scr, mkpos(12, 5));
    carriage_return(&scr);
    update(result, tposeq("carriage_return", mkpos(0, 5), cursor(&scr)));

    scr = screen(80, 25);
    cursor_address(&scr, mkpos(12, 5));
    newline(&scr);
    update(result, tposeq("newline", mkpos(0, 6), cursor(&scr)));

    scr = screen(80, 25);
    cursor_address(&scr, mkpos(10, 6));
    tab(&scr);
    update(result, tposeq("tab", mkpos(16, 6), cursor(&scr)));

    scr = screen(80, 25);
    cursor_address(&scr, mkpos(1, 6));
    column_address(&scr, 16);
    update(result, tposeq("column_address", mkpos(16, 6), cursor(&scr)));

    scr = screen(80, 25);
    cursor_address(&scr, mkpos(16, 6));
    row_address(&scr, 12);
    update(result, tposeq("row_address", mkpos(16, 12), cursor(&scr)));

    scr = screen(80, 25);
    cursor_address(&scr, mkpos(15, 6));
    cursor_down(&scr);
    update(result, tposeq("cursor_down", mkpos(15, 7), cursor(&scr)));

    scr = screen(80, 25);
    cursor_address(&scr, mkpos(15, 6));
    cursor_home(&scr);
    update(result, tposeq("cursor_home", mkpos(0, 0), cursor(&scr)));

    scr = screen(80, 25);
    cursor_address(&scr, mkpos(6, 8));
    cursor_left(&scr);
    update(result, tposeq("cursor_left", mkpos(5, 8), cursor(&scr)));

    scr = screen(80, 25);
    cursor_address(&scr, mkpos(6, 8));
    cursor_right(&scr);
    update(result, tposeq("cursor_right", mkpos(7, 8), cursor(&scr)));

    scr = screen(80, 25);
    cursor_address(&scr, mkpos(6, 8));
    cursor_to_ll(&scr);
    update(result, tposeq("cursor_to_ll", mkpos(0, 24), cursor(&scr)));

    scr = screen(80, 25);
    cursor_address(&scr, mkpos(6, 8));
    cursor_up(&scr);
    update(result, tposeq("cursor_up", mkpos(6, 7), cursor(&scr)));

    scr = screen(80, 25);
    cursor_address(&scr, mkpos(26, 9));
    parm_left_cursor(&scr, 23);
    update(result, tposeq("parm_left_cursor", mkpos(3, 9), cursor(&scr)));

    scr = screen(80, 25);
    cursor_address(&scr, mkpos(3, 9));
    parm_right_cursor(&scr, 23);
    update(result, tposeq("parm_right_cursor", mkpos(26, 9), cursor(&scr)));

    scr = screen(80, 25);
    cursor_address(&scr, mkpos(26, 4));
    parm_down_cursor(&scr, 11);
    update(result, tposeq("parm_down_cursor", mkpos(26, 15), cursor(&scr)));

    scr = screen(6, 4);
    cursor_address(&scr, mkpos(2, 3));
    put_char(&scr, 'a');
    SCREEN_Cell want_0 = {'a', SCREEN_COLOR_WHITE, SCREEN_COLOR_BLACK, {false, false}};
    update(result, tcelleq("put_char puts", want_0, cellat(&scr, mkpos(2,3))));

    scr = screen(6, 4);
    cursor_address(&scr, mkpos(2, 3));
    put_char(&scr, 'a');
    update(result, tposeq("put_char moves", mkpos(3, 3), cursor(&scr)));

    scr = screen(6, 4);
    cursor_address(&scr, mkpos(5, 1));
    put_char(&scr, 'a');
    update(result, tposeq("put_char am", mkpos(0, 2), cursor(&scr)));

    scr = screen(6, 4);
    put_chars(&scr, "abcdefGHIJKLmno");
    SCREEN_Cell want_1 = {'J', SCREEN_COLOR_WHITE, SCREEN_COLOR_BLACK, {false, false}};
    update(result, tcelleq("put_char multiple", want_1, cellat(&scr, mkpos(3,1))));

    scr = screen(6, 4);
    put_chars(&scr, "abcdefGHIJKLmno");
    clear_screen(&scr);
    update(result, tposeq("clear_screen homes", mkpos(0, 0), cursor(&scr)));

    scr = screen(6, 4);
    put_chars(&scr, "abcdefGHIJKLmno");
    clear_screen(&scr);
    SCREEN_Cell want_2 = {' ', SCREEN_COLOR_WHITE, SCREEN_COLOR_BLACK, {false, false}};
    update(result, tcelleq("clear_screen clears", want_2, cellat(&scr, mkpos(3,1))));

    scr = screen(6, 4);
    cursor_address(&scr, mkpos(0, 3));
    put_chars(&scr, "abcdefGHIJKLmno");
    SCREEN_Cell want_3 = {'J', SCREEN_COLOR_WHITE, SCREEN_COLOR_BLACK, {false, false}};
    update(result, tcelleq("put_char scrolls at end", want_3, cellat(&scr, mkpos(3,2))));

    scr = screen(6, 3);
    put_chars(&scr, "abcdefGHIJKLmno");
    cursor_home(&scr);
    clr_eos(&scr);
    SCREEN_Cell want_4 = {' ', SCREEN_COLOR_WHITE, SCREEN_COLOR_BLACK, {false, false}};
    update(result, tcelleq("put_char scrolls at end", want_4, cellat(&scr, mkpos(4,1))));
    
    scr = screen(6, 4);
    cursor_down(&scr);
    put_chars(&scr, "abcdefGHIJKLmno");
    cursor_address(&scr, mkpos(0, 3));
    newline(&scr);
    SCREEN_Cell want_5 = {'K', SCREEN_COLOR_WHITE, SCREEN_COLOR_BLACK, {false, false}};
    update(result, tcelleq("newline at bottom scrolls", want_5, cellat(&scr, mkpos(4,1))));

    scr = screen(6, 4);
    put_chars(&scr, "abcdefGHIJKLmno");
    cursor_address(&scr, mkpos(0, 0));
    insert_line(&scr);
    SCREEN_Cell want_6 = {' ', SCREEN_COLOR_WHITE, SCREEN_COLOR_BLACK, {false, false}};
    update(result, tcelleq("insert_line clears full line", want_6, cellat(&scr, mkpos(5,0))));

    scr = screen(24, 4);
    cursor_address(&scr, mkpos(20, 1));
    tab(&scr);
    update(result, tposeq("tab at eol", mkpos(0, 2), cursor(&scr)));
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


