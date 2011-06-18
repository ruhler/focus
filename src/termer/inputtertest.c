
// Copyright (C) 2011 Richard Uhler
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
        CNSL_MakeKeypress(CNSLK_a), CNSL_MakeKeyrelease(CNSLK_a),
        CNSL_MakeQuit()
    };
    update(result, streq("a", "a", try(a)));

    CNSL_Event abcd[] = {
        CNSL_MakeKeypress(CNSLK_a), CNSL_MakeKeyrelease(CNSLK_a),
        CNSL_MakeKeypress(CNSLK_b), CNSL_MakeKeyrelease(CNSLK_b),
        CNSL_MakeKeypress(CNSLK_c), CNSL_MakeKeyrelease(CNSLK_c),
        CNSL_MakeKeypress(CNSLK_d), CNSL_MakeKeyrelease(CNSLK_d),
        CNSL_MakeQuit()
    };
    update(result, streq("abcd", "abcd", try(abcd)));

    CNSL_Event aBc[] = {
        CNSL_MakeKeypress(CNSLK_a), CNSL_MakeKeyrelease(CNSLK_a),
        CNSL_MakeKeypress(CNSLK_LSHIFT),
        CNSL_MakeKeypress(CNSLK_b), CNSL_MakeKeyrelease(CNSLK_b),
        CNSL_MakeKeyrelease(CNSLK_LSHIFT),
        CNSL_MakeKeypress(CNSLK_c), CNSL_MakeKeyrelease(CNSLK_c),
        CNSL_MakeQuit()
    };
    update(result, streq("aBc", "aBc", try(aBc)));

    CNSL_Event mm[] = {
        CNSL_MakeKeypress(CNSLK_MINUS), CNSL_MakeKeyrelease(CNSLK_MINUS),
        CNSL_MakeKeypress(CNSLK_MINUS), CNSL_MakeKeyrelease(CNSLK_MINUS),
        CNSL_MakeQuit()
    };
    update(result, streq("--", "--", try(mm)));
    
    CNSL_Event semicolon[] = {
        CNSL_MakeKeypress(CNSLK_SEMICOLON), CNSL_MakeKeyrelease(CNSLK_SEMICOLON),
        CNSL_MakeQuit()
    };
    update(result, streq("semicolon", ";", try(semicolon)));

    CNSL_Event t0123[] = {
        CNSL_MakeKeypress(CNSLK_0), CNSL_MakeKeyrelease(CNSLK_0),
        CNSL_MakeKeypress(CNSLK_1), CNSL_MakeKeyrelease(CNSLK_1),
        CNSL_MakeKeypress(CNSLK_2), CNSL_MakeKeyrelease(CNSLK_2),
        CNSL_MakeKeypress(CNSLK_3), CNSL_MakeKeyrelease(CNSLK_3),
        CNSL_MakeQuit()
    };
    update(result, streq("0123", "0123", try(t0123)));

    CNSL_Event arrows[] = {
        CNSL_MakeKeypress(CNSLK_UP), CNSL_MakeKeyrelease(CNSLK_UP),
        CNSL_MakeKeypress(CNSLK_DOWN), CNSL_MakeKeyrelease(CNSLK_DOWN),
        CNSL_MakeKeypress(CNSLK_LEFT), CNSL_MakeKeyrelease(CNSLK_LEFT),
        CNSL_MakeKeypress(CNSLK_RIGHT), CNSL_MakeKeyrelease(CNSLK_RIGHT),
        CNSL_MakeQuit()
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

