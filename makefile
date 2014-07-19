
TARGETS := all install install-html install-dvi install-pdf install-ps \
	uninstall install-strip clean distclean mostlyclean maintainer-clean \
	TAGS info dvi html pdf ps dist check installcheck installdirs

.PHONY: $(TARGETS)
$(TARGETS):
	tclsh make.tcl --target $@

