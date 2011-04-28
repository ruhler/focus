
#include <CairoOutputDev.h>
#include <GlobalParams.h>

#include "Document.h"

Document::Document(PDFDoc* doc)
    : m_doc(doc)
{}

Document::~Document()
{
    delete m_doc;
}

Document* Document::load(const std::string& filename)
{
    if (!globalParams) {
        // This memory will never be freed, but that's okay, because we expect
        // it to last the entire run of the program anyway.
        globalParams = new GlobalParams();
    }

    // The PDFDoc assumes responsibility for the new GooString.
    PDFDoc* doc = new PDFDoc(new GooString(filename.c_str()));
    if (!doc->isOk()) {
        delete doc;
        return NULL;
    }

    return new Document(doc);
}

void Document::draw(cairo_t* cairo, int pagenum, double zoom, const std::string& highlight)
{
    // Clear the background
    cairo_set_source_rgb(cairo, 1.0, 1.0, 1.0);
    cairo_paint(cairo);

    // Highlight the text
    if (!highlight.empty()) {
        TextOutputDev* todev = new TextOutputDev(NULL, gTrue, gFalse, gFalse);
        m_doc->displayPage(todev, pagenum, 72/zoom, 72/zoom, 0, gFalse, gTrue, gFalse);

        Unicode* u = (Unicode *)gmallocn(highlight.size(), sizeof(Unicode));
        for (int i = 0; i < highlight.size(); ++i) {
            u[i] = (Unicode)(highlight[i] & 0xff);
        }

        double xmin = 0;
        double ymin = 0;
        double xmax = 0;
        double ymax = 0;

        GBool starttop = gTrue;
        while (todev->findText(u, highlight.size(), starttop, 1, ~starttop, 0, 0, 0, &xmin, &ymin, &xmax, &ymax)) {
            starttop = gFalse;

            cairo_rectangle(cairo, xmin, ymin, xmax-xmin, ymax-ymin);
            cairo_set_source_rgb(cairo, 1, 1, 0);
            cairo_fill(cairo);
        }

        delete todev;
        gfree(u);
    }

    // Draw the page content.
    CairoOutputDev dev;
    dev.setCairo(cairo);
    dev.startDoc(m_doc->getXRef(), m_doc->getCatalog());
    m_doc->displayPage(&dev, pagenum, 72.0/zoom, 72.0/zoom, 0, 0, 0, 0);
}

int Document::pages()
{
    return m_doc->getNumPages();
}

double Document::width(int pagenum)
{
    return m_doc->getPageMediaWidth(pagenum);
}

double Document::height(int pagenum)
{
    return m_doc->getPageMediaHeight(pagenum);
}

int Document::search(std::string str, int page)
{
    TextOutputDev* todev = new TextOutputDev(NULL, gFalse, gFalse, gFalse);

    Unicode* u = (Unicode *)gmallocn(str.size(), sizeof(Unicode));
    for (int i = 0; i < str.size(); ++i) {
        u[i] = (Unicode)(str[i] & 0xff);
    }

    double xmin = 0;
    double ymin = 0;
    double xmax = 0;
    double ymax = 0;

    int found;
    for(found = -1; found < 1 && page <= pages(); page++) {
        m_doc->displayPage(todev, page, 72, 72, 0, gFalse, gTrue, gFalse);
        if (todev->findText(u, str.size(), gTrue, 1, gFalse, 0, 0, 0, &xmin, &ymin, &xmax, &ymax)) {
            found = page;
        }
    }

    delete todev;
    gfree(u);
    return found;
}

