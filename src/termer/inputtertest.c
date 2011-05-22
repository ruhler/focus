
#include <stdio.h>

#include "inputter.h"

bool streq(const char* name, const char* a, const char* b)
{
    if (strcmp(a, b) != 0) {
        printf("%s: expected: %s, got: %s\n", name, a, b);
        return false;
    }
    return true;
}

void update(bool* result, bool x)
{
    *result = *result && x;
}

char trybuf[BUFSIZ];
char* tryputpoint;
CNSL_Event* eventbuf;

CNSL_Event tryget()
{
    CNSL_Event* x = eventbuf;
    eventbuf++;
    return *x;
}

void tryput(char c)
{
    *tryputpoint = c;
    tryputpoint++;
}

char* try(CNSL_Event* events)
{
    trybuf[0] = '\0';
    tryputpoint = trybuf;
    eventbuf = events;
    inputter(tryget, tryput);
    *tryputpoint = '\0';
    return trybuf;
}

void test(bool* result)
{
    CNSL_Event a[] = {
        {CNSLE_KEYPRESS, CNSLK_a}, {CNSLE_KEYRELEASE, CNSLK_a},
        {CNSLE_QUIT, 0}
    };
    update(result, streq("a", "a", try(a)));

    CNSL_Event abcd[] = {
        {CNSLE_KEYPRESS, CNSLK_a}, {CNSLE_KEYRELEASE, CNSLK_a},
        {CNSLE_KEYPRESS, CNSLK_b}, {CNSLE_KEYRELEASE, CNSLK_b},
        {CNSLE_KEYPRESS, CNSLK_c}, {CNSLE_KEYRELEASE, CNSLK_c},
        {CNSLE_KEYPRESS, CNSLK_d}, {CNSLE_KEYRELEASE, CNSLK_d},
        {CNSLE_QUIT, 0}
    };
    update(result, streq("abcd", "abcd", try(abcd)));

    CNSL_Event aBc[] = {
        {CNSLE_KEYPRESS, CNSLK_a}, {CNSLE_KEYRELEASE, CNSLK_a},
        {CNSLE_KEYPRESS, CNSLK_LSHIFT},
        {CNSLE_KEYPRESS, CNSLK_b}, {CNSLE_KEYRELEASE, CNSLK_b},
        {CNSLE_KEYRELEASE, CNSLK_LSHIFT},
        {CNSLE_KEYPRESS, CNSLK_c}, {CNSLE_KEYRELEASE, CNSLK_c},
        {CNSLE_QUIT, 0}
    };
    update(result, streq("aBc", "aBc", try(aBc)));

    CNSL_Event mm[] = {
        {CNSLE_KEYPRESS, CNSLK_MINUS}, {CNSLE_KEYRELEASE, CNSLK_MINUS},
        {CNSLE_KEYPRESS, CNSLK_MINUS}, {CNSLE_KEYRELEASE, CNSLK_MINUS},
        {CNSLE_QUIT, 0}
    };
    update(result, streq("--", "--", try(mm)));
    
    CNSL_Event semicolon[] = {
        {CNSLE_KEYPRESS, CNSLK_SEMICOLON}, {CNSLE_KEYRELEASE, CNSLK_SEMICOLON},
        {CNSLE_QUIT, 0}
    };
    update(result, streq("semicolon", ";", try(semicolon)));

    CNSL_Event t0123[] = {
        {CNSLE_KEYPRESS, CNSLK_0}, {CNSLE_KEYRELEASE, CNSLK_0},
        {CNSLE_KEYPRESS, CNSLK_1}, {CNSLE_KEYRELEASE, CNSLK_1},
        {CNSLE_KEYPRESS, CNSLK_2}, {CNSLE_KEYRELEASE, CNSLK_2},
        {CNSLE_KEYPRESS, CNSLK_3}, {CNSLE_KEYRELEASE, CNSLK_3},
        {CNSLE_QUIT, 0}
    };
    update(result, streq("0123", "0123", try(t0123)));

    CNSL_Event arrows[] = {
        {CNSLE_KEYPRESS, CNSLK_UP}, {CNSLE_KEYRELEASE, CNSLK_UP},
        {CNSLE_KEYPRESS, CNSLK_DOWN}, {CNSLE_KEYRELEASE, CNSLK_DOWN},
        {CNSLE_KEYPRESS, CNSLK_LEFT}, {CNSLE_KEYRELEASE, CNSLK_LEFT},
        {CNSLE_KEYPRESS, CNSLK_RIGHT}, {CNSLE_KEYRELEASE, CNSLK_RIGHT},
        {CNSLE_QUIT, 0}
    };
    update(result, streq("arrows", "\e[A\e[B\e[D\e[C", try(arrows)));

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

