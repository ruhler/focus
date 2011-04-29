
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

