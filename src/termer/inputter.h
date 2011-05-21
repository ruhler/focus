
#ifndef INPUTTER_H
#define INPUTTER_H

#include "consoler.h"

typedef int bool;

extern bool true;
extern bool false;


typedef CNSL_Event(*GetEventFunction)();
typedef void(*PutCharFunction)(char);

#endif//INPUTTER_H

