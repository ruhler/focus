
SUBDIRS = sdlcsr termer

doc_DATA = focus.html

focus.html: focus.txt
	asciidoc -o $@ -a VERSION=$(VERSION) $<
