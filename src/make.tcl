
execv asciidoc -o focus.html -a VERSION=$::VERSION focus.txt
execv asciidoc -o README.html README.txt

if $::INSTALL {
    install $::PREFIX/share/doc/focus-$::VERSION focus.html
}

