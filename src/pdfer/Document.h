
#ifndef DOCUMENT_H
#define DOCUMENT_H

#include <string>

#include <cairo.h>
#include <PDFDoc.h>

class Document
{
public:
    ~Document();

    // Load the pdf document from the named file.
    // The document object returned is allocated with new and must be
    // deallocated with delete when it is no longer needed.
    // Returns NULL if the document couldn't be loaded.
    static Document* load(const std::string& filename);

    // Draw the given page to the cairo context
    // Greater zoom means a smaller page.
    // Instances of the string highlight will be highlighted in the draw
    // image if it is a non-empty string.
    void draw(cairo_t* cairo, int pagenum, double zoom, const std::string& highlight);

    // Search for the given string in the document starting at the given page.
    // Returns the first page which contains an instance of str.
    // Returns -1 if there is no such page.
    int search(std::string str, int pagenum);

    // Return the number of pages in the document.
    int pages();

    double width(int pagenum);
    double height(int pagenum);

private:
    // doc will be allocated with new. This assigns responsibility to the
    // Document to delete doc when it's done with it.
    Document(PDFDoc* doc);

    // Don't allow copying.
    Document(const Document& rhs);

    PDFDoc* m_doc;
};

#endif//DOCUMENT_H

