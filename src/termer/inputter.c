
#include <assert.h>
#include <stdio.h>

#include "inputter.h"

typedef struct {
    bool shifton;
    bool ctrlon;
} InputterState;

void sinputter(InputterState* is, GetEventFunction iget, PutCharFunction iput);
void press(InputterState* is, PutCharFunction iput, CNSL_Keysym key, bool shift, bool ctrl);
void release(InputterState* is, CNSL_Keysym key);

InputterState initial()
{
    InputterState is;
    is.shifton = false;
    is.ctrlon = false;
    return is;
}

void setshift(InputterState* is, bool x)
{
    is->shifton = x;
}

void setctrl(InputterState* is, bool x)
{
    is->ctrlon = x;
}

void inputter(GetEventFunction iget, PutCharFunction iput)
{
    InputterState is = initial();
    sinputter(&is, iget, iput);
}

void sinputter(InputterState* is, GetEventFunction iget, PutCharFunction iput)
{
    while (1) {
        CNSL_Event event = iget();
        CNSL_Keysym sym;
        bool shift = is->shifton;
        bool ctrl = is->ctrlon;

        if (CNSL_IsQuit(event)) {
            return;
        } else if (CNSL_IsKeypress(event, &sym)) {
            press(is, iput, sym, shift, ctrl);
        } else if (CNSL_IsKeyrelease(event, &sym)) {
            release(is, sym);
        }
    }
}

void many(PutCharFunction iput, const char* str)
{
    while (*str) {
        iput(*str);
        str++;
    }
}

void press(InputterState* is, PutCharFunction iput, CNSL_Keysym key, bool shift, bool ctrl)
{
    switch (key) {
        case CNSLK_LSHIFT: setshift(is, true); return;
        case CNSLK_RSHIFT: setshift(is, true); return;
        case CNSLK_LCTRL: setctrl(is, true); return;
        case CNSLK_RCTRL: setctrl(is, true); return;
        case CNSLK_BACKSPACE: iput('\b'); return;
        case CNSLK_TAB: iput('\t'); return;
        case CNSLK_RETURN: iput('\r'); return;
        case CNSLK_ESCAPE: iput(0x1b); return;
        case CNSLK_SPACE: ctrl ? iput('\0') : iput(' '); return;
        case CNSLK_0: shift ? iput(')') : iput('0'); return;
        case CNSLK_1: shift ? iput('!') : iput('1'); return;
        case CNSLK_2: shift ? iput('@') : iput('2'); return;
        case CNSLK_3: shift ? iput('#') : iput('3'); return;
        case CNSLK_4: shift ? iput('$') : iput('4'); return;
        case CNSLK_5: shift ? iput('%') : iput('5'); return;
        case CNSLK_6: shift ? iput('^') : iput('6'); return;
        case CNSLK_7: shift ? iput('&') : iput('7'); return;
        case CNSLK_8: shift ? iput('*') : iput('8'); return;
        case CNSLK_9: shift ? iput('(') : iput('9'); return;
        case CNSLK_a: ctrl ? iput(1) : (shift ? iput('A') : iput('a')); return;
        case CNSLK_b: ctrl ? iput(2) : (shift ? iput('B') : iput('b')); return;
        case CNSLK_c: ctrl ? iput(3) : (shift ? iput('C') : iput('c')); return;
        case CNSLK_d: ctrl ? iput(4) : (shift ? iput('D') : iput('d')); return;
        case CNSLK_e: ctrl ? iput(5) : (shift ? iput('E') : iput('e')); return;
        case CNSLK_f: ctrl ? iput(6) : (shift ? iput('F') : iput('f')); return;
        case CNSLK_g: ctrl ? iput(7) : (shift ? iput('G') : iput('g')); return;
        case CNSLK_h: ctrl ? iput(8) : (shift ? iput('H') : iput('h')); return;
        case CNSLK_i: ctrl ? iput(9) : (shift ? iput('I') : iput('i')); return;
        case CNSLK_j: ctrl ? iput(10) : (shift ? iput('J') : iput('j')); return;
        case CNSLK_k: ctrl ? iput(11) : (shift ? iput('K') : iput('k')); return;
        case CNSLK_l: ctrl ? iput(12) : (shift ? iput('L') : iput('l')); return;
        case CNSLK_m: ctrl ? iput(13) : (shift ? iput('M') : iput('m')); return;
        case CNSLK_n: ctrl ? iput(14) : (shift ? iput('N') : iput('n')); return;
        case CNSLK_o: ctrl ? iput(15) : (shift ? iput('O') : iput('o')); return;
        case CNSLK_p: ctrl ? iput(16) : (shift ? iput('P') : iput('p')); return;
        case CNSLK_q: ctrl ? iput(17) : (shift ? iput('Q') : iput('q')); return;
        case CNSLK_r: ctrl ? iput(18) : (shift ? iput('R') : iput('r')); return;
        case CNSLK_s: ctrl ? iput(19) : (shift ? iput('S') : iput('s')); return;
        case CNSLK_t: ctrl ? iput(20) : (shift ? iput('T') : iput('t')); return;
        case CNSLK_u: ctrl ? iput(21) : (shift ? iput('U') : iput('u')); return;
        case CNSLK_v: ctrl ? iput(22) : (shift ? iput('V') : iput('v')); return;
        case CNSLK_w: ctrl ? iput(23) : (shift ? iput('W') : iput('w')); return;
        case CNSLK_x: ctrl ? iput(24) : (shift ? iput('X') : iput('x')); return;
        case CNSLK_y: ctrl ? iput(25) : (shift ? iput('Y') : iput('y')); return;
        case CNSLK_z: ctrl ? iput(26) : (shift ? iput('Z') : iput('z')); return;
        case CNSLK_LEFTBRACKET: ctrl ? iput(0x1b) : (shift ? iput('{') : iput('[')); return;
        case CNSLK_BACKSLASH: ctrl ? iput(0x1c) : (shift ? iput('|') : iput('\\')); return;
        case CNSLK_RIGHTBRACKET: ctrl ? iput(0x1d) : (shift ? iput('}') : iput(']')); return;
        case CNSLK_BACKQUOTE: ctrl ? iput(0x1e) : (shift ? iput('~') : iput('`')); return;
        case CNSLK_SLASH: ctrl ? iput(0x1f) : (shift ? iput('?') : iput('/')); return;
        case CNSLK_MINUS: shift ? iput('_') : iput('-'); return;
        case CNSLK_EQUALS: shift ? iput('+') : iput('='); return;
        case CNSLK_SEMICOLON: shift ? iput(':') : iput(';'); return;
        case CNSLK_COMMA: shift ? iput('<') : iput(','); return;
        case CNSLK_PERIOD: shift ? iput('>') : iput('.'); return;
        case CNSLK_QUOTE: shift ? iput('"') : iput('\''); return;
        case CNSLK_UP: many(iput, "\e[A"); return;
        case CNSLK_DOWN: many(iput, "\e[B"); return;
        case CNSLK_RIGHT: many(iput, "\e[C"); return;
        case CNSLK_LEFT: many(iput, "\e[D"); return;
        case CNSLK_HOME: many(iput, "\e[H"); return;
        case CNSLK_END: many(iput, "\e[4~"); return;
        case CNSLK_DELETE: iput(0x7F); return;
        default:
            fprintf(stderr, "ignoring unhandled input key: %i\n", key);
            return;
    }
}

void release(InputterState* is, CNSL_Keysym key)
{
    switch (key) {
        case CNSLK_RSHIFT: setshift(is, false); return;
        case CNSLK_LSHIFT: setshift(is, false); return;
        case CNSLK_RCTRL: setctrl(is, false); return;
        case CNSLK_LCTRL: setctrl(is, false); return;
        default: return;
    }
}

