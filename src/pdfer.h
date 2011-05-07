
#ifndef PDFER_H
#define PDFER_H

extern "C" {
    #include "consoler.h"
}


#include <poppler-document.h>
#include <poppler-image.h>


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

    // Actions on the view
    // goto the given page. The first page is page 1.
    // Does nothing if the page number is not valid.
    void goto_(int page);

    // goto the next page
    // does nothing if you are at the last page.
    void next();

    // goto the previous page
    // does nothing if you are at the first page.
    void previous();

    // scroll the page forward by yp% and left by xp%
    // xp and yp can be negative to scroll backward or right respectively.
    // For example, to scroll forward by 10% of the view height, you would
    // call: scroll(0, 0.1);
    void scroll(double xp, double yp);

    // Zoom out by the given factor.
    // for example, zoom(2.0) zooms out, zoom(0.5) zooms in.
    void zoom(double zf);

    // Fit the page into the full width of the view.
    void fitwidth();

    // Fit the entire page into the view
    void fitpage();

private:
    Pdfer(poppler::document* doc, int width, int height);

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
};

#endif//PDFER_H

