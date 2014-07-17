
execv asciidoc -o focus.html -a VERSION=$::VERSION focus.txt

if $::INSTALL {
    install -t $::PREFIX/share/doc/focus-$::VERSION focus.html
}

