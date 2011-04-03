
#include <stdio.h>
#include <stdlib.h>

#include "ccl.h"

Buffer ccl_alloc_buffer(unsigned int width, unsigned int height)
{
    Buffer buf = malloc(sizeof(Buffer_));
    if (!buf) {
        return NULL;
    }

    buf->width = width;
    buf->height = height;
    buf->pixels = malloc(sizeof(Color) * width * height);
    if (!buf->pixels) {
        free(buf);
        return NULL;
    }
    return buf;
}

void ccl_free_buffer(Buffer buffer)
{
    free(buffer->pixels);
    free(buffer);
}

void ccl_blit(Buffer src,
        unsigned int srcx, unsigned int srcy,
        unsigned int dstx, unsigned int dsty,
        unsigned int width, unsigned int height)
{
    // Output the data using the following format:
    // ui4: x destination
    // ui4: y destination
    // ui4: width
    // ui4: height
    // width*ui4: row 1
    // width*ui4: row 2
    // ...
    // width*ui4: row (height)

    unsigned int header[] = {dstx, dsty, width, height};
    fwrite(header, 4, 4, stdout);

    int x;
    int y;
    for (y = srcy; y < height+srcy; y++) {
        for (x = srcx; x < width + srcx; x++) {
            Color c = ccl_getpixel(src, x, y);
            fwrite(&c, 4, 1, stdout);
        }
    }
    fflush(stdout);
}

Color ccl_getpixel(Buffer buffer, unsigned int x, unsigned int y)
{
    return buffer->pixels[y*buffer->width + x];
}

void ccl_setpixel(Buffer buffer, unsigned int x, unsigned int y, Color c)
{
    buffer->pixels[y*buffer->width + x] = c;
}

Color ccl_rgb8(int r, int g, int b)
{
    return (r << 16) | (g << 8) | b;
}


int ccl_redof(Color c)
{
    return 0xFF & (c >> 16);
}

int ccl_greenof(Color c)
{
    return 0xFF & (c >> 8);
}

int ccl_blueof(Color c)
{
    return 0xFF & c;
}

void ccl_event(Event* event)
{
    // Event
    // One of:
    // PHH - key press with code in hex
    // RHH - key release with code in hex.
    if (feof(stdin)) {
        fprintf(stderr, "client: got end of file\n");
        exit(0);
    }

    fread(event, sizeof(Event), 1, stdin);
}

int ccl_keypress(const Event* e, int* code)
{
    if (e->type == EVENT_KEYPRESS) {
        *code = e->value;
    }
    return e->type == EVENT_KEYPRESS;
}

int ccl_keyrelease(const Event* e, int* code)
{
    if (e->type == EVENT_KEYRELEASE) {
        *code = e->value;
    }
    return e->type == EVENT_KEYRELEASE;
}

