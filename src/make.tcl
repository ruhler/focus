
make::all {
    execv asciidoc -o focus.html -a VERSION=$::VERSION focus.txt
    execv asciidoc -o README.html README.txt
}

make::install cmd::install $::PREFIX/share/doc/focus-$::VERSION focus.html
make::clean execv rm -f focus.html README.html

