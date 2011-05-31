
#ifndef KMAPPER_H
#define KMAPPER_H

// An action to perform when a key sequence is pressed.
//  ud - user data from when the action was registered.
typedef void (*KMPR_Action)(void* ud);

// A KMPR_Action which does nothing.
void KMPR_NullAction(void* ud)
{
    return ();
}

typedef struct {
    KMPR_Action action;
    void* userdata
} KMPR_ActionClosure;

// KMPR_KMapper is a pointer to a 256 element array of Action Closures,
// indexed by character.
typedef KMPR_ActionClosure* KMPR_KMapper;

// Register an action to be called when the given character is pressed.
void KMPR_RegisterAction(KMPR_KMapper kmapper, char c, KMPR_Action action, void* userdata);

#endif//KMAPPER_H

