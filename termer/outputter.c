
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

#include <ctype.h>
#include <stdio.h>

#include "outputter.h"

void getnum_aux(GetCharFunction getf, int x, int* iout, char* cout);

int digit2int(char c)
{
    return c - '0';
}

// Given a function to get the next character in an input stream,
//  return the next integer in the input stream and following character.
//  If there is not an integer at the front of the stream, returns false
//  and the integer value is not updated.
bool getnum(GetCharFunction getf, int* iout, char* cout)
{
    char c = getf();
    if (isdigit(c)) {
        getnum_aux(getf, digit2int(c), iout, cout);
        return true;
    } else {
        *cout = c;
        return false;
    }
}

void getnum_aux(GetCharFunction getf, int x, int* iout, char* cout)
{
    char c = getf();
    if (isdigit(c)) {
        getnum_aux(getf, x*10 + digit2int(c), iout, cout);
    } else {
        *iout = x;
        *cout = c;
    }
}

void mode(SCREEN_Screen* scr, int x)
{
    switch (x) {
        case 0: exit_attribute_mode(scr); break;
        case 1: enter_bold_mode(scr); break;
        case 7: enter_reverse_mode(scr); break;
        case 10: break;
        case 22: exit_bold_mode(scr); break;
        case 30: set_foreground(scr, SCREEN_COLOR_BLACK); break;
        case 31: set_foreground(scr, SCREEN_COLOR_RED); break;
        case 32: set_foreground(scr, SCREEN_COLOR_GREEN); break;
        case 33: set_foreground(scr, SCREEN_COLOR_YELLOW); break;
        case 34: set_foreground(scr, SCREEN_COLOR_BLUE); break;
        case 35: set_foreground(scr, SCREEN_COLOR_MAGENTA); break;
        case 36: set_foreground(scr, SCREEN_COLOR_CYAN); break;
        case 37: set_foreground(scr, SCREEN_COLOR_WHITE); break;
        case 39: set_foreground(scr, SCREEN_COLOR_WHITE); break;
        case 40: set_background(scr, SCREEN_COLOR_BLACK); break;
        case 41: set_background(scr, SCREEN_COLOR_RED); break;
        case 42: set_background(scr, SCREEN_COLOR_GREEN); break;
        case 43: set_background(scr, SCREEN_COLOR_YELLOW); break;
        case 44: set_background(scr, SCREEN_COLOR_BLUE); break;
        case 45: set_background(scr, SCREEN_COLOR_MAGENTA); break;
        case 46: set_background(scr, SCREEN_COLOR_CYAN); break;
        case 47: set_background(scr, SCREEN_COLOR_WHITE); break;
        case 49: set_background(scr, SCREEN_COLOR_BLACK); break;
        default: 
            fprintf(stderr, "uknown mode parameter: %i\n", x);
            break;
    }
}

void outputter(SCREEN_Screen* scr, int terminator, GetCharFunction getf)
{
    while (1) {
        unsigned char c = getf();
        if (c == terminator) {
            return;
        }

        switch (c) {
            case '\a': break;   // TODO: alert?
            case '\t': tab(scr); break;
            case '\n': cursor_down(scr); break;
            case '\b': cursor_left(scr); break;
            case '\r': carriage_return(scr); break;
            case 0x1b: 
            {
                c = getf();
                switch (c) {
                    case '[':
                    {
                        int x;
                        if (getnum(getf, &x, &c)) {
                            if (x == 1 && c == 'K') {
                                clr_bol(scr);
                            } else {
                                switch (c) {
                                    case 'm': mode(scr, x); break;
                                    case 'A': parm_up_cursor(scr, x); break;
                                    case 'B': parm_down_cursor(scr, x); break;
                                    case 'C': parm_right_cursor(scr, x); break;
                                    case 'D': parm_left_cursor(scr, x); break;
                                    case 'G': column_address(scr, (x-1)); break;
                                    case 'L': parm_insert_line(scr, x); break;
                                    case 'M': parm_delete_line(scr, x); break;
                                    case 'P': parm_dch(scr, x); break;
                                    case 'S': parm_index(scr, x); break;
                                    case 'T': parm_rindex(scr, x); break;
                                    case 'X': erase_chars(scr, x); break;
                                    case '@': parm_ich(scr, x); break;
                                    case 'd': row_address(scr, (x-1)); break;
                                    case ';':
                                    {
                                        int y;
                                        if (getnum(getf, &y, &c)) {
                                            switch (c) {
                                                case 'm':
                                                    mode(scr, x);
                                                    mode(scr, y);
                                                    break;

                                                case 'H': cursor_address(scr, mkpos(y-1, x-1)); break;
                                                default: 
                                                    fprintf(stderr, "unhandled: ESC[%i;%i%c\n", x, y, c);
                                                    break;
                                            }
                                        } else {
                                            fprintf(stderr, "unhandled: ESC[%i;%c\n", x, c);
                                        }
                                    } break;

                                    default:
                                        fprintf(stderr, "unahndled: ESC[%i%c\n", x, c);
                                        break;
                                }
                            }
                        } else {
                            switch (c) {
                                case '@': insert_character(scr); break;
                                case 'A': cursor_up(scr); break;
                                case 'B': cursor_down(scr); break;
                                case 'C': cursor_right(scr); break;
                                case 'D': cursor_left(scr); break;
                                case 'H': cursor_home(scr); break;
                                case 'I': tab(scr); break;
                                case 'J': clr_eos(scr); break;
                                case 'K': clr_eol(scr); break;
                                case 'L': insert_line(scr); break;
                                case 'M': delete_line(scr); break;
                                case 'P': delete_character(scr); break;
                                case 'm': exit_attribute_mode(scr); break;
                                default:
                                    fprintf(stderr, "unhandled: ESC[%c\n", c);
                                    break;
                            }
                        }

                    } break;

                    case 'M':
                        scroll_reverse(scr);
                        break;

                    default:
                        fprintf(stderr, "unhandled: ESC%c\n", c);
                        break;
                }
            } break;

            default:
            {
                wchar_t wc;
                if ((c & 0x80) == 0x00) {
                    wc = c;
                } else if ((c & 0xE0) == 0xC0) {
                    char c1 = 0x1F & c;
                    char c2 = 0x3F & getf();
                    wc = (c1 << 6) | c2;
                } else if ((c & 0xF0) == 0xE0) {
                    char c1 = 0x0F & c;
                    char c2 = 0x3F & getf();
                    char c3 = 0x3F & getf();
                    wc = (c1 << 12) | (c2 << 6) | c3;
                } else if ((c & 0xF8) == 0xF0) {
                    char c1 = 0x07 & c;
                    char c2 = 0x3F & getf();
                    char c3 = 0x3F & getf();
                    char c4 = 0x3F & getf();
                    wc = (c1 << 18) | (c2 << 12) | (c3 << 6) | c4;
                } else if ((c & 0xFC) == 0xF8) {
                    char c1 = 0x03 & c;
                    char c2 = 0x3F & getf();
                    char c3 = 0x3F & getf();
                    char c4 = 0x3F & getf();
                    char c5 = 0x3F & getf();
                    wc = (c1 << 24) | (c2 << 18) | (c3 << 12) | (c4 << 6) | c5;
                } else if ((c & 0xFE) == 0xFC) {
                    char c1 = 0x01 & c;
                    char c2 = 0x3F & getf();
                    char c3 = 0x3F & getf();
                    char c4 = 0x3F & getf();
                    char c5 = 0x3F & getf();
                    char c6 = 0x3F & getf();
                    wc = (c1 << 30) | (c2 << 24) | (c3 << 18) | (c4 << 12) | (c5 < 6) | c6;
                } else {
                    fprintf(stderr, "bad utf-8 sequence: c=0x%02x\n", c);
                    wc = '\0';
                }

                put_char(scr, wc);
            } break;
        }
    }
}

