
#include "consoler.h"


int CNSL_redof(CNSL_Color c);
int CNSL_blueof(CNSL_Color c);
int CNSL_greenof(CNSL_Color c);
CNSL_Color CNSL_rgb8(int r, int g, int b);

CNSL_Display CNSL_alloc_display(unsigned int width, unsigned int height) {
    CNSL_Display display = malloc(sizeof(CNSL_Display_));
    if (!display) {
        return NULL;
    }

    display->width = width;
    display->height = height;
    display->pixels = malloc(sizeof(CNSL_Color) * width * height);
    if (!display->pixels) {
        free(display);
        return NULL;
    }
    return display;
}


void CNSL_free_display(CNSL_Display display)
{
    if (display) {
        free(display->pixels);
        free(display);
    }
}


CNSL_Color CNSL_getpixel(CNSL_Display display, unsigned int x, unsigned int y);
void CNSL_setpixel(CNSL_Display display, unsigned int x, unsigned int y, CNSL_Color color);
int CNSL_iskeypress(const CNSL_Event* e, int* code);
int CNSL_iskeyrelease(const CNSL_Event* e, int* code);
extern CNSL_Console stdcon;
CNSL_Client CNSL_launch_client(const char* path, char* const args[]);
void CNSL_close_client(CNSL_Client client);
int CNSL_event_recv(CNSL_Console console, CNSL_Event* event);
int CNSL_event_send(CNSL_Client client, const CNSL_Event* event);
int CNSL_display_send(CNSL_Console console, CNSL_Display display,
        unsigned int srcx, unsigned int srcy,
        unsigned int dstx, unsigned int dsty,
        unsigned int width, unsigned int height);
int CNSL_display_recv(CNSL_Client client,
        unsigned int* dstx, unsigned int* dsty,
        unsigned int* width, unsigned int* height,
        CNSL_DRFunction f, void* ud);


