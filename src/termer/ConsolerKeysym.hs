
module ConsolerKeysym (Keysym(..))
  where

data Keysym = 
    BACKSPACE | TAB | CLEAR | RETURN | PAUSE | ESCAPE
  | SPACE | EXCLAIM | QUOTEDBL | HASH | DOLLAR | AMPERSAND | QUOTE
  | LEFTPAREN | RIGHTPAREN | ASTERISK | PLUS | COMMA | MINUS | PERIOD | SLASH
  | D0 | D1 | D2 | D3 | D4 | D5 | D6 | D7 | D8 | D9
  | COLON | SEMICOLON | LESS | EQUALS | GREATER | QUESTION | AT
  | LEFTBRACKET | BACKSLASH | RIGHTBRACKET | CARET | UNDERSCORE | BACKQUOTE
  | A | B | C | D | E | F | G | H | I | J | K | L | M | N | O | P
  | Q | R | S | T | U | V | W | X | Y | Z
  | DELETE | KP0 | KP1 | KP2 | KP3 | KP4 | KP5 | KP6 | KP7 | KP8 | KP9
  | KP_PERIOD | KP_DIVIDE | KP_MULTIPLY | KP_MINUS | KP_PLUS | KP_ENTER
  | KP_EQUALS | UP | DOWN | RIGHT | LEFT | INSERT | HOME | END | PAGEUP
  | PAGEDOWN | F1 | F2 | F3 | F4 | F5 | F6 | F7 | F8 | F9 | F10 | F11
  | F12 | F13 | F14 | F15 | NUMLOCK | CAPSLOCK | SCROLLOCK | RSHIFT
  | LSHIFT | RCTRL | LCTRL | RALT | LALT | RMETA | LMETA | LSUPER
  | RSUPER | MODE | COMPOSE | HELP | PRINT | SYSREQ | BREAK | MENU
  | POWER | EURO | UNDO deriving(Eq, Show)

instance Enum Keysym
  where
    fromEnum BACKSPACE = 8
    fromEnum TAB = 9
    fromEnum CLEAR = 12
    fromEnum RETURN = 13
    fromEnum PAUSE = 19
    fromEnum ESCAPE = 27
    fromEnum SPACE = 32
    fromEnum EXCLAIM = 33
    fromEnum QUOTEDBL = 34
    fromEnum HASH = 35
    fromEnum DOLLAR = 36
    fromEnum AMPERSAND = 38
    fromEnum QUOTE = 39
    fromEnum LEFTPAREN = 40
    fromEnum RIGHTPAREN = 41
    fromEnum ASTERISK = 42
    fromEnum PLUS = 43
    fromEnum COMMA = 44
    fromEnum MINUS = 45
    fromEnum PERIOD = 46
    fromEnum SLASH = 47
    fromEnum D0 = 48
    fromEnum D1 = 49
    fromEnum D2 = 50
    fromEnum D3 = 51
    fromEnum D4 = 52
    fromEnum D5 = 53
    fromEnum D6 = 54
    fromEnum D7 = 55
    fromEnum D8 = 56
    fromEnum D9 = 57
    fromEnum COLON = 58
    fromEnum SEMICOLON = 59
    fromEnum LESS = 60
    fromEnum EQUALS = 61
    fromEnum GREATER = 62
    fromEnum QUESTION = 63
    fromEnum AT = 64
    fromEnum LEFTBRACKET = 91
    fromEnum BACKSLASH = 92
    fromEnum RIGHTBRACKET = 93
    fromEnum CARET = 94
    fromEnum UNDERSCORE = 95
    fromEnum BACKQUOTE = 96
    fromEnum A = 97
    fromEnum B = 98
    fromEnum C = 99
    fromEnum D = 100
    fromEnum E = 101
    fromEnum F = 102
    fromEnum G = 103
    fromEnum H = 104
    fromEnum I = 105
    fromEnum J = 106
    fromEnum K = 107
    fromEnum L = 108
    fromEnum M = 109
    fromEnum N = 110
    fromEnum O = 111
    fromEnum P = 112
    fromEnum Q = 113
    fromEnum R = 114
    fromEnum S = 115
    fromEnum T = 116
    fromEnum U = 117
    fromEnum V = 118
    fromEnum W = 119
    fromEnum X = 120
    fromEnum Y = 121
    fromEnum Z = 122
    fromEnum DELETE = 127
    fromEnum KP0 = 256
    fromEnum KP1 = 257
    fromEnum KP2 = 258
    fromEnum KP3 = 259
    fromEnum KP4 = 260
    fromEnum KP5 = 261
    fromEnum KP6 = 262
    fromEnum KP7 = 263
    fromEnum KP8 = 264
    fromEnum KP9 = 265
    fromEnum KP_PERIOD = 266
    fromEnum KP_DIVIDE = 267
    fromEnum KP_MULTIPLY = 268
    fromEnum KP_MINUS = 269
    fromEnum KP_PLUS = 270
    fromEnum KP_ENTER = 271
    fromEnum KP_EQUALS = 272
    fromEnum UP = 273
    fromEnum DOWN = 274
    fromEnum RIGHT = 275
    fromEnum LEFT = 276
    fromEnum INSERT = 277
    fromEnum HOME = 278
    fromEnum END = 279
    fromEnum PAGEUP = 280
    fromEnum PAGEDOWN = 281
    fromEnum F1 = 282
    fromEnum F2 = 283
    fromEnum F3 = 284
    fromEnum F4 = 285
    fromEnum F5 = 286
    fromEnum F6 = 287
    fromEnum F7 = 288
    fromEnum F8 = 289
    fromEnum F9 = 290
    fromEnum F10 = 291
    fromEnum F11 = 292
    fromEnum F12 = 293
    fromEnum F13 = 294
    fromEnum F14 = 295
    fromEnum F15 = 296
    fromEnum NUMLOCK = 300
    fromEnum CAPSLOCK = 301
    fromEnum SCROLLOCK = 302
    fromEnum RSHIFT = 303
    fromEnum LSHIFT = 304
    fromEnum RCTRL = 305
    fromEnum LCTRL = 306
    fromEnum RALT = 307
    fromEnum LALT = 308
    fromEnum RMETA = 309
    fromEnum LMETA = 310
    fromEnum LSUPER = 311
    fromEnum RSUPER = 312
    fromEnum MODE = 313
    fromEnum COMPOSE = 314
    fromEnum HELP = 315
    fromEnum PRINT = 316
    fromEnum SYSREQ = 317
    fromEnum BREAK = 318
    fromEnum MENU = 319
    fromEnum POWER = 320
    fromEnum EURO = 321
    fromEnum UNDO = 322

    toEnum 8 = BACKSPACE
    toEnum 9 = TAB
    toEnum 12 = CLEAR
    toEnum 13 = RETURN
    toEnum 19 = PAUSE
    toEnum 27 = ESCAPE
    toEnum 32 = SPACE
    toEnum 33 = EXCLAIM
    toEnum 34 = QUOTEDBL
    toEnum 35 = HASH
    toEnum 36 = DOLLAR
    toEnum 38 = AMPERSAND
    toEnum 39 = QUOTE
    toEnum 40 = LEFTPAREN
    toEnum 41 = RIGHTPAREN
    toEnum 42 = ASTERISK
    toEnum 43 = PLUS
    toEnum 44 = COMMA
    toEnum 45 = MINUS
    toEnum 46 = PERIOD
    toEnum 47 = SLASH
    toEnum 48 = D0
    toEnum 49 = D1
    toEnum 50 = D2
    toEnum 51 = D3
    toEnum 52 = D4
    toEnum 53 = D5
    toEnum 54 = D6
    toEnum 55 = D7
    toEnum 56 = D8
    toEnum 57 = D9
    toEnum 58 = COLON
    toEnum 59 = SEMICOLON
    toEnum 60 = LESS
    toEnum 61 = EQUALS
    toEnum 62 = GREATER
    toEnum 63 = QUESTION
    toEnum 64 = AT
    toEnum 91 = LEFTBRACKET
    toEnum 92 = BACKSLASH
    toEnum 93 = RIGHTBRACKET
    toEnum 94 = CARET
    toEnum 95 = UNDERSCORE
    toEnum 96 = BACKQUOTE
    toEnum 97 = A
    toEnum 98 = B
    toEnum 99 = C
    toEnum 100 = D
    toEnum 101 = E
    toEnum 102 = F
    toEnum 103 = G
    toEnum 104 = H
    toEnum 105 = I
    toEnum 106 = J
    toEnum 107 = K
    toEnum 108 = L
    toEnum 109 = M
    toEnum 110 = N
    toEnum 111 = O
    toEnum 112 = P
    toEnum 113 = Q
    toEnum 114 = R
    toEnum 115 = S
    toEnum 116 = T
    toEnum 117 = U
    toEnum 118 = V
    toEnum 119 = W
    toEnum 120 = X
    toEnum 121 = Y
    toEnum 122 = Z
    toEnum 127 = DELETE
    toEnum 256 = KP0
    toEnum 257 = KP1
    toEnum 258 = KP2
    toEnum 259 = KP3
    toEnum 260 = KP4
    toEnum 261 = KP5
    toEnum 262 = KP6
    toEnum 263 = KP7
    toEnum 264 = KP8
    toEnum 265 = KP9
    toEnum 266 = KP_PERIOD
    toEnum 267 = KP_DIVIDE
    toEnum 268 = KP_MULTIPLY
    toEnum 269 = KP_MINUS
    toEnum 270 = KP_PLUS
    toEnum 271 = KP_ENTER
    toEnum 272 = KP_EQUALS
    toEnum 273 = UP
    toEnum 274 = DOWN
    toEnum 275 = RIGHT
    toEnum 276 = LEFT
    toEnum 277 = INSERT
    toEnum 278 = HOME
    toEnum 279 = END
    toEnum 280 = PAGEUP
    toEnum 281 = PAGEDOWN
    toEnum 282 = F1
    toEnum 283 = F2
    toEnum 284 = F3
    toEnum 285 = F4
    toEnum 286 = F5
    toEnum 287 = F6
    toEnum 288 = F7
    toEnum 289 = F8
    toEnum 290 = F9
    toEnum 291 = F10
    toEnum 292 = F11
    toEnum 293 = F12
    toEnum 294 = F13
    toEnum 295 = F14
    toEnum 296 = F15
    toEnum 300 = NUMLOCK
    toEnum 301 = CAPSLOCK
    toEnum 302 = SCROLLOCK
    toEnum 303 = RSHIFT
    toEnum 304 = LSHIFT
    toEnum 305 = RCTRL
    toEnum 306 = LCTRL
    toEnum 307 = RALT
    toEnum 308 = LALT
    toEnum 309 = RMETA
    toEnum 310 = LMETA
    toEnum 311 = LSUPER
    toEnum 312 = RSUPER
    toEnum 313 = MODE
    toEnum 314 = COMPOSE
    toEnum 315 = HELP
    toEnum 316 = PRINT
    toEnum 317 = SYSREQ
    toEnum 318 = BREAK
    toEnum 319 = MENU
    toEnum 320 = POWER
    toEnum 321 = EURO
    toEnum 322 = UNDO

