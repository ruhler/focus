
SUBDIRS = consoler fonter boxer filler green imager pdfer sdlcsr termer

doc_DATA = focus.html

focus.html: focus.txt
	asciidoc -o $@ -a VERSION=$(VERSION) $<
