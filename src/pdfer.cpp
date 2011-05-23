
#include <stdio.h>

#include "pdfer.h"

#include <poppler-page.h>
#include <poppler-page-renderer.h>
#include <poppler-rectangle.h>

Pdfer::Pdfer(poppler::document* doc, int width, int height)
    : m_width(width), m_height(height), m_doc(doc),
      m_page(1), m_zoom(1.0), m_x(0.0), m_y(0.0)
{
    redraw();
}

Pdfer* Pdfer::load(const std::string& filename, int width, int height)
{
    poppler::document* doc = poppler::document::load_from_file(filename);
    if (!doc) {
        return NULL;
    }

    return new Pdfer(doc, width, height);
}

void Pdfer::unload(Pdfer* pdfer)
{
    delete pdfer;
}

void Pdfer::show(CNSL_Display display)
{
    int x = (int)m_x;
    int y = (int)m_y;
    int sw = m_image.width();
    int sh = m_image.height();

    unsigned int* pixels = (unsigned int*)m_image.data();

    // (c, r) are coordinates in the display
    for (int r = 0; r < display->height; r++) {
        for (int c = 0; c < display->width; c++) {
            if (y + r >= 0 && y + r < sh && x + c >= 0 && x + c < sw) {
                unsigned int pixel = pixels[(y+r)*sw + (x+c)];
                CNSL_SetPixel(display, c, r, pixel);
            } else {
                // Background color: grey
                CNSL_SetPixel(display, c, r, CNSL_MakeColor(0x80, 0x80, 0x80));
            }
        }
    }
}

void Pdfer::goto_(int page)
{
    if (page >= 1 && page <= m_doc->pages()) {
        m_page = page;
        redraw();
    }
    m_y = 0;
}

void Pdfer::next()
{
    goto_(m_page + 1);
}

void Pdfer::previous()
{
    goto_(m_page - 1);
}

void Pdfer::scroll(double xp, double yp)
{
    m_x -= xp * m_width;
    m_y -= yp * m_width;
}

void Pdfer::zoom(double zf)
{
    m_zoom *= zf;
    m_x = (m_x + m_width/2)/zf - m_width/2;
    m_y = (m_y + m_height/2)/zf - m_height/2;
    redraw();
}

void Pdfer::fitwidth()
{
    m_zoom = pagewidth()/m_width;
    m_x = 0;
    redraw();
}

void Pdfer::fitpage()
{
    double wz = pagewidth()/m_width;
    double hz = pageheight()/m_height;

    // center the image.
    zoom(1.0/m_zoom);
    m_x = (pagewidth() - m_width)/2.0;
    m_y = (pageheight() - m_height)/2.0;

    zoom(std::max(wz, hz));
}

void Pdfer::redraw()
{
    // For poppler first page is 0, so we have to adjust.
    // TODO: is it bad to recreate pages? Should we cache them instead?
    poppler::page* page = m_doc->create_page(m_page-1);

    // TODO: can or should we reuse the renderer for every page rather than
    // creating a new one for every page?
    poppler::page_renderer renderer;
    renderer.set_paper_color(0xFFFFFF);
    m_image = renderer.render_page(page, 72.0/m_zoom, 72.0/m_zoom);
}

double Pdfer::pagewidth()
{
    return m_doc->create_page(m_page-1)->page_rect().width();
}

double Pdfer::pageheight()
{
    return m_doc->create_page(m_page-1)->page_rect().height();
}

