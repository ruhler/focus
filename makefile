
.PHONY: default
default:
	tclsh make.tcl

.PHONY: clean
clean:
	rm -v `cat .gitignore`

