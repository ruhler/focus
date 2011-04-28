
#include <cassert>
#include <iostream>
#include <string>


extern "C" {
#include "consoler.h"
}

#include "Document.h"

const int WIDTH = 640;
const int HEIGHT = 480;

// Draw the given view to a new cairo_surface and return the surface.
// The surface should be destroyed when you are done with it.
cairo_surface_t* draw(Document* doc, int page, double zoom)
{
    int w = doc->width(page)/zoom;
    int h = doc->height(page)/zoom;
    cairo_surface_t* surface = cairo_image_surface_create(CAIRO_FORMAT_ARGB32, w, h);
    cairo_t* cairo = cairo_create(surface);
    doc->draw(cairo, page, zoom, "");
    cairo_destroy(cairo);
    return surface;
}

// Show the page on the display, with x and y the coordinates of the surface
// to be placed at the upper left corner of the display.
// x and y may be negative.
void show(CNSL_Display display, cairo_surface_t* surface, int x, int y)
{
    int sw = cairo_image_surface_get_width(surface);
    int sh = cairo_image_surface_get_height(surface);
    unsigned int* pixels = (unsigned int*)cairo_image_surface_get_data(surface);

    // (c, r) are coordinates in the display
    for (int r = 0; r < HEIGHT; r++) {
        for (int c = 0; c < WIDTH; c++) {
            if (y + r >= 0 && y + r < sh && x + c >= 0 && x + c < sw) {
                unsigned int pixel = pixels[(y+r)*sw + (x+c)];
                int red = (pixel >> 16) & 0xFF;
                int green = (pixel >> 8) & 0xFF;
                int blue = (pixel) & 0xFF;
                CNSL_SetPixel(display, c, r, CNSL_MakeColor(red, green, blue));
            } else {
                // Background color: grey
                CNSL_SetPixel(display, c, r, CNSL_MakeColor(0x80, 0x80, 0x80));
            }
        }
    }
    CNSL_SendDisplay(stdcon, display, 0, 0, 0, 0, WIDTH, HEIGHT);
}

int main(int argc, char* argv[])
{
    if (argc < 2) {
        std::cerr << "no input file" << std::endl;
        return 1;
    }

    std::string pdffilename = argv[1];

    Document* doc = Document::load(pdffilename);
    if (!doc) {
        std::cerr << "Error loading pdf " << pdffilename << std::endl;
        return 1;
    }

    CNSL_Init();
    CNSL_Display display = CNSL_AllocDisplay(WIDTH, HEIGHT);

    int page = 1;
    double zoom = 1.0;
    double x = 0;
    double y = 0;

    cairo_surface_t* surface = draw(doc, page, zoom);
    show(display, surface, (int)x, (int)y);

    CNSL_Event event;
    bool done = false;
    
    while (!done) {
        CNSL_RecvEvent(stdcon, &event);
        int sym;
        bool redraw = false;
        bool reshow = false;

        if (CNSL_IsKeypress(&event, &sym)) {
            switch (sym) {
                case CNSLK_q:
                    done = true;
                    break;

                case CNSLK_SPACE:
                case CNSLK_n:
                    if (page+1 <= doc->pages()) {
                        page++;
                        redraw = true;
                    }
                    break;

                case CNSLK_p:
                    if (page-1 > 0) {
                        page--;
                        redraw = true;
                    }
                    break;

                case CNSLK_j:
                    y += 10;
                    reshow = true;
                    break;

                case CNSLK_k:
                    y -= 10;
                    reshow = true;
                    break;

                case CNSLK_h:
                    x -= 10;
                    reshow = true;
                    break;

                case CNSLK_l:
                    x += 10;
                    reshow = true;
                    break;

                case CNSLK_i:
                    zoom *= 0.8;
                    x = (x + WIDTH/2)/0.8 - WIDTH/2;
                    y = (y + HEIGHT/2)/0.8 - HEIGHT/2;
                    redraw = true;
                    break;

                case CNSLK_o:
                    zoom *= 1.25;
                    x = (x + WIDTH/2)/1.25 - WIDTH/2;
                    y = (y + HEIGHT/2)/1.25 - HEIGHT/2;
                    redraw = true;
                    break;

                case CNSLK_w:
                    zoom = doc->width(page) / WIDTH;
                    x = 0;
                    redraw = true;
                    break;

                case CNSLK_a:
                {
                    double wzoom = doc->width(page)/WIDTH;
                    double hzoom = doc->height(page)/HEIGHT;
                    x = (doc->width(page) - WIDTH)/2.0;
                    y = (doc->height(page) - HEIGHT)/2.0;
                    double amt = std::max(wzoom, hzoom);
                    zoom *= amt;
                    x = (x + WIDTH/2)/amt - WIDTH/2;
                    y = (y + HEIGHT/2)/amt - HEIGHT/2;
                    redraw = true;

                } break;
            }
        }

        if (redraw) {
            cairo_surface_destroy(surface);
            surface = draw(doc, page, zoom);
            reshow = true;
        }

        if (reshow) {
            show(display, surface, (int)x, (int)y);
        }
    }

    CNSL_Quit();
    return 0;
}

