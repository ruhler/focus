
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

#ifndef CNSL_KEYSYM_H
#define CNSL_KEYSYM_H

// keysyms specified to match SDL.

typedef int CNSL_Keysym;

#define CNSLK_BACKSPACE 8
#define CNSLK_TAB 9
#define CNSLK_CLEAR 12
#define CNSLK_RETURN 13
#define CNSLK_PAUSE 19
#define CNSLK_ESCAPE 27
#define CNSLK_SPACE 32
#define CNSLK_EXCLAIM 33
#define CNSLK_QUOTEDBL 34
#define CNSLK_HASH 35
#define CNSLK_DOLLAR 36
#define CNSLK_AMPERSAND 38
#define CNSLK_QUOTE 39
#define CNSLK_LEFTPAREN 40
#define CNSLK_RIGHTPAREN 41
#define CNSLK_ASTERISK 42
#define CNSLK_PLUS 43
#define CNSLK_COMMA 44
#define CNSLK_MINUS 45
#define CNSLK_PERIOD 46
#define CNSLK_SLASH 47
#define CNSLK_0  48
#define CNSLK_1  49
#define CNSLK_2  50
#define CNSLK_3  51
#define CNSLK_4  52
#define CNSLK_5  53
#define CNSLK_6  54
#define CNSLK_7  55
#define CNSLK_8  56
#define CNSLK_9  57
#define CNSLK_COLON 58
#define CNSLK_SEMICOLON 59
#define CNSLK_LESS 60
#define CNSLK_EQUALS 61
#define CNSLK_GREATER 62
#define CNSLK_QUESTION 63
#define CNSLK_AT  64
#define CNSLK_LEFTBRACKET 91
#define CNSLK_BACKSLASH 92
#define CNSLK_RIGHTBRACKET 93
#define CNSLK_CARET 94
#define CNSLK_UNDERSCORE 95
#define CNSLK_BACKQUOTE 96
#define CNSLK_a  97
#define CNSLK_b  98
#define CNSLK_c  99
#define CNSLK_d  100
#define CNSLK_e  101
#define CNSLK_f  102
#define CNSLK_g  103
#define CNSLK_h  104
#define CNSLK_i  105
#define CNSLK_j  106
#define CNSLK_k  107
#define CNSLK_l  108
#define CNSLK_m  109
#define CNSLK_n  110
#define CNSLK_o  111
#define CNSLK_p  112
#define CNSLK_q  113
#define CNSLK_r  114
#define CNSLK_s  115
#define CNSLK_t  116
#define CNSLK_u  117
#define CNSLK_v  118
#define CNSLK_w  119
#define CNSLK_x  120
#define CNSLK_y  121
#define CNSLK_z  122
#define CNSLK_DELETE 127
#define CNSLK_KP0 256
#define CNSLK_KP1 257
#define CNSLK_KP2 258
#define CNSLK_KP3 259
#define CNSLK_KP4 260
#define CNSLK_KP5 261
#define CNSLK_KP6 262
#define CNSLK_KP7 263
#define CNSLK_KP8 264
#define CNSLK_KP9 265
#define CNSLK_KP_PERIOD 266
#define CNSLK_KP_DIVIDE 267
#define CNSLK_KP_MULTIPLY = 268
#define CNSLK_KP_MINUS 269
#define CNSLK_KP_PLUS 270
#define CNSLK_KP_ENTER 271
#define CNSLK_KP_EQUALS 272
#define CNSLK_UP  273
#define CNSLK_DOWN 274
#define CNSLK_RIGHT 275
#define CNSLK_LEFT 276
#define CNSLK_INSERT 277
#define CNSLK_HOME 278
#define CNSLK_END 279
#define CNSLK_PAGEUP 280
#define CNSLK_PAGEDOWN 281
#define CNSLK_F1  282
#define CNSLK_F2  283
#define CNSLK_F3  284
#define CNSLK_F4  285
#define CNSLK_F5  286
#define CNSLK_F6  287
#define CNSLK_F7  288
#define CNSLK_F8  289
#define CNSLK_F9  290
#define CNSLK_F10 291
#define CNSLK_F11 292
#define CNSLK_F12 293
#define CNSLK_F13 294
#define CNSLK_F14 295
#define CNSLK_F15 296
#define CNSLK_NUMLOCK 300
#define CNSLK_CAPSLOCK 301
#define CNSLK_SCROLLOCK 302
#define CNSLK_RSHIFT 303
#define CNSLK_LSHIFT 304
#define CNSLK_RCTRL 305
#define CNSLK_LCTRL 306
#define CNSLK_RALT 307
#define CNSLK_LALT 308
#define CNSLK_RMETA 309
#define CNSLK_LMETA 310
#define CNSLK_LSUPER 311
#define CNSLK_RSUPER 312
#define CNSLK_MODE 313
#define CNSLK_COMPOSE 314
#define CNSLK_HELP 315
#define CNSLK_PRINT 316
#define CNSLK_SYSREQ 317
#define CNSLK_BREAK 318
#define CNSLK_MENU 319
#define CNSLK_POWER 320
#define CNSLK_EURO 321
#define CNSLK_UNDO 322

// This should always be the last keysym (it's a fake).
#define CNSLK_LAST 323

#endif//CNSL_KEYSYM_H

