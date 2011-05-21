
#ifndef OUTPUTTER_H
#define OUTPUTTER_H

#include "screen.h"

typedef char (GetCharFunction)();

void outputter(SCREEN_Screen* scr, int terminator, GetCharFunction getf);

#endif//OUTPUTTER_H

