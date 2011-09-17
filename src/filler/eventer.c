
#include <stdlib.h>

#include "eventer.h"

struct EventerNode {
    CNSL_Event event;
    const char* action;
    struct EventerNode* next;
};

// An Eventer is a pointer to the (possibly NULL) head of a  singly linked
// list of EventerNodes.

Eventer Eventer_Create()
{
    Eventer eventer = malloc(sizeof(struct EventerNode*));
    if (eventer == NULL) {
        return NULL;
    }

    *eventer = NULL;
    return eventer;
}

void Eventer_Free(Eventer eventer)
{
    struct EventerNode* ptr = *eventer;
    while (ptr != NULL) {
        struct EventerNode* next = ptr->next;
        free(ptr);
        ptr = next;
    }

    free(eventer);
}

void Eventer_Insert(Eventer eventer, CNSL_Event event, const char* action)
{
    struct EventerNode* head = malloc(sizeof(struct EventerNode));
    head->event = event;
    head->action = action;
    head->next = *eventer;
    *eventer = head;
}

const char* Eventer_Lookup(Eventer eventer, CNSL_Event event)
{
    struct EventerNode* ptr = *eventer;
    while (ptr != NULL) {
        if (CNSL_EventsEqual(ptr->event, event)) {
            return ptr->action;
        }
        ptr = ptr->next;
    }
    return NULL;
}

