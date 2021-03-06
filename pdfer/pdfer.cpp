
// Copyright (C) 2011 Richard Uhler <ruhler@member.fsf.org>
//
// This file is part of Focus.
//
// Focus is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.
//
// Focus is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with Focus.  If not, see <http://www.gnu.org/licenses/>.

#include <sstream>

#include "pdfer.h"

#include <poppler-page.h>
#include <poppler-page-renderer.h>
#include <poppler-rectangle.h>

Pdfer::Pdfer(const std::string& filename, poppler::document* doc, int width, int height)
    : m_width(width), m_height(height), m_doc(doc),
      m_page(1), m_zoom(1.0), m_x(0.0), m_y(0.0), m_rotation(0),
      m_fonter(FNTR_Create("Monospace-24:Bold")), m_status(false),
      m_filename(filename)
{
    redraw();
}

Pdfer* Pdfer::load(const std::string& filename, int width, int height)
{
    poppler::document* doc = poppler::document::load_from_file(filename);
    if (!doc) {
        return NULL;
    }

    return new Pdfer(filename, doc, width, height);
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
    for (int r = 0; r < display.height; r++) {
        for (int c = 0; c < display.width; c++) {
            if (y + r >= 0 && y + r < sh && x + c >= 0 && x + c < sw) {
                unsigned int pixel = pixels[(y+r)*sw + (x+c)];
                CNSL_SetPixel(display, c, r, pixel);
            } else {
                // Background color: grey
                CNSL_SetPixel(display, c, r, CNSL_MakeColor(0x80, 0x80, 0x80));
            }
        }
    }

    if (m_status) {
        // Draw the status bar.
        CNSL_Color fg = CNSL_MakeColor(0xFF, 0xFF, 0xFF);
        CNSL_Color bg = CNSL_MakeColor(0x00, 0x00, 0x80);
        std::ostringstream oss;
        oss << m_filename << "    " << page() << " of " << pages() << " ";
        FNTR_DrawString(m_fonter, display, fg, bg, 0, m_height - FNTR_Height(m_fonter), oss.str().c_str());
    }
}

void Pdfer::status()
{
    m_status = true;
}

void Pdfer::unstatus()
{
    m_status = false;
}

void Pdfer::goto_(int page)
{
    if (page >= 1 && page <= m_doc->pages()) {
        m_page = page;
        redraw();
        top();
    } else {
        status();
    }
}

void Pdfer::next()
{
    goto_(m_page + 1);
}

void Pdfer::previous()
{
    goto_(m_page - 1);
}

void Pdfer::first()
{
    goto_(1);
}

void Pdfer::last()
{
    goto_(m_doc->pages());
}

void Pdfer::scroll(double xp, double yp)
{
    m_x -= xp * m_width;
    m_y -= yp * m_height;
}

void Pdfer::top()
{
    m_y = 0;
}

void Pdfer::bottom()
{
    m_y = pageheight()/m_zoom - m_height;
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

// Given an angle in degrees, return the corresponding angle bounded within 
// the range [0, 360)
int normalizeangle(int x)
{
    while (x < 0) {
        x += 360;
    }

    while (x > 360) {
        x -= 360;
    }
    return x;
}

void Pdfer::rotate(int x)
{
    m_rotation += x;
    redraw();
}

int Pdfer::page()
{
    return m_page;
}

int Pdfer::pages()
{
    return m_doc->pages();
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
    m_image = renderer.render_page(page, 72.0/m_zoom, 72.0/m_zoom,
            -1, -1, -1, -1, (poppler::rotation_enum)(normalizeangle(m_rotation)/90));
}

double Pdfer::pagewidth()
{
    int rotate = m_rotation;
    switch (m_doc->create_page(m_page-1)->orientation()) {
        case poppler::page::landscape: rotate += 90; break;
        case poppler::page::portrait: rotate += 0; break;
        case poppler::page::seascape: rotate += 270; break;
        case poppler::page::upside_down: rotate += 180; break;
    }

    if (normalizeangle(rotate) % 180 == 90) {
        return m_doc->create_page(m_page-1)->page_rect().height();
    }
    return m_doc->create_page(m_page-1)->page_rect().width();
}

double Pdfer::pageheight()
{
    int rotate = m_rotation;
    switch (m_doc->create_page(m_page-1)->orientation()) {
        case poppler::page::landscape: rotate += 90; break;
        case poppler::page::portrait: rotate += 0; break;
        case poppler::page::seascape: rotate += 270; break;
        case poppler::page::upside_down: rotate += 180; break;
    }

    if (normalizeangle(rotate) % 180 == 90) {
        return m_doc->create_page(m_page-1)->page_rect().width();
    }
    return m_doc->create_page(m_page-1)->page_rect().height();
}

void Pdfer::resize(int width, int height)
{
    m_width = width;
    m_height = height;
}

