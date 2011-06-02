
#include <assert.h>
#include <stdlib.h>

#include "kmapper.h"

void KMPR_NullAction(void* ud)
{
    return;
}

KMPR_KMapper KMPR_Create()
{
    KMPR_KMapper kmapper = malloc(CNSLK_LAST * sizeof(KMPR_ActionClosure));
    if (!kmapper) {
        return NULL;
    }

    int i;
    for (i = 0; i < CNSLK_LAST; i++) {
        kmapper[i].action = KMPR_NullAction;
        kmapper[i].userdata = NULL;
    }

    return kmapper;
}

void KMPR_Free(KMPR_KMapper kmapper)
{
    free(kmapper);
}

void KMPR_RegisterAction(KMPR_KMapper kmapper, CNSL_Keysym key, KMPR_Action action, void* userdata)
{
    kmapper[key].action = action;
    kmapper[key].userdata = userdata;
}

void KMPR_NextEvent(KMPR_KMapper kmapper, const CNSL_Event* event)
{
    CNSL_Keysym sym;
    if (CNSL_IsKeypress(event, &sym)) {
        assert(sym < CNSLK_LAST);
        kmapper[sym].action(kmapper[sym].userdata);
    }
}

