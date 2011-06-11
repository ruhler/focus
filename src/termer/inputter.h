
#ifndef INPUTTER_H
#define INPUTTER_H

#include "consoler.h"

typedef CNSL_Event(*GetEventFunction)();
typedef void(*PutCharFunction)(char);

void inputter(GetEventFunction iget, PutCharFunction iput);

#endif//INPUTTER_H

