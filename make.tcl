
source tclmk.tcl
source config.tcl
source librarydoc.tcl

indir consoler {source make.tcl}
indir fonter {source make.tcl}
indir boxer {source make.tcl}
indir filler {source make.tcl}
indir green {source make.tcl}
indir imager {source make.tcl}
indir pdfer {source make.tcl}
indir sdlcsr {source make.tcl}
indir termer {source make.tcl}

make::all {
    execv asciidoc -o focus.html -a VERSION=$::VERSION focus.txt
    execv asciidoc -o README.html README.txt
}

make::install cmd::install $::PREFIX/share/doc/focus-$::VERSION focus.html
make::clean execv rm -f focus.html README.html

