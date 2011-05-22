
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
        char c = getf();
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
                                        fprintf(stderr, "unahndled: ESC[%i%c\n", c);
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

                    default:
                        fprintf(stderr, "unhandled: ESC%c\n", c);
                        break;
                }
            } break;

            default: put_char(scr, c); break;
        }
    }
}

