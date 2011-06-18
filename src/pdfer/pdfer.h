
// Copyright (C) 2011 Richard Uhler
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

#ifndef PDFER_H
#define PDFER_H

#include <poppler-document.h>
#include <poppler-image.h>

extern "C" {
    #include "consoler.h"
    #include "fonter.h"
}

class Pdfer
{
public:
    // Load a pdf file into a new Pdfer view of given dimensions.
    // Returns NULL if there was an error.
    // Must be cleaned up with unload function when you are done.
    static Pdfer* load(const std::string& filename, int width, int height);
    static void unload(Pdfer* pdfer);

    // Draw the view to the given display.
    void show(CNSL_Display display);

    // turn on the status bar
    void status();

    // turn off the status bar
    void unstatus();
    
    // goto the given page. The first page is page 1.
    // Does nothing if the page number is not valid.
    void goto_(int page);

    // goto the next page
    // does nothing if you are at the last page.
    void next();

    // goto the previous page
    // does nothing if you are at the first page.
    void previous();

    // goto the first page.
    void first();

    // goto the last page.
    void last();

    // scroll the page forward by yp% and left by xp%
    // xp and yp can be negative to scroll backward or right respectively.
    // For example, to scroll forward by 10% of the view height, you would
    // call: scroll(0, 0.1);
    void scroll(double xp, double yp);

    // scroll to the top of the current page
    void top();

    // scroll to the bottom of the current page
    void bottom();

    // Zoom out by the given factor.
    // for example, zoom(2.0) zooms out, zoom(0.5) zooms in.
    void zoom(double zf);

    // Fit the page into the full width of the view.
    void fitwidth();

    // Fit the entire page into the view
    void fitpage();

    // Current page number
    int page();

    // Total number of pages.
    int pages();

private:
    Pdfer(const std::string& filename, poppler::document* doc, int width, int height);

    // Given the current page and zoom, redraw the page, updating m_image with
    // the result.
    void redraw();

    // Return the width and height of the current page.
    double pagewidth();
    double pageheight();

    // width and height are dimensions of the view (not the page)
    int m_width;
    int m_height;

    poppler::document* m_doc;

    // m_image is the full drawing of the current page at the given zoom.
    poppler::image m_image;

    // first page is page 1.
    int m_page;
    double m_zoom;

    // m_x and m_y are the position in the page which is shown at the upper
    // left corner of the view (they may be negative).
    double m_x;
    double m_y;

    // fonter object for drawing status bar
    FNTR_Fonter m_fonter;

    // true if the status bar is on, false otherwise.
    bool m_status;
    std::string m_filename;
};

#endif//PDFER_H

