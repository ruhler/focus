
#ifndef KMAPPER_H
#define KMAPPER_H

#include "consoler.h"

// An action to perform when a key sequence is pressed.
//  ud - user data from when the action was registered.
typedef void (*KMPR_Action)(void* ud);

// A KMPR_Action which does nothing.
void KMPR_NullAction(void* ud);

typedef struct {
    KMPR_Action action;
    void* userdata;
} KMPR_ActionClosure;

// KMPR_KMapper is an array of Action Closures indexed by keysym.
typedef KMPR_ActionClosure* KMPR_KMapper;

// Create a new key mapper object.
// This should be freed with KMPR_Free when you're done with it.
KMPR_KMapper KMPR_Create();

// Free a created key mapper object.
void KMPR_Free(KMPR_KMapper kmapper);

// Register an action to be called when the given keysym is pressed.
// Overrides previously resgistered action.
void KMPR_RegisterAction(KMPR_KMapper kmapper, CNSL_Keysym key, KMPR_Action action, void* userdata);

// Tell the keymapper of the next event (the event may be of any type).
void KMPR_NextEvent(KMPR_KMapper kmapper, const CNSL_Event* event);

#endif//KMAPPER_H

