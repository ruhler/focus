
execv gcc -o boxer -I../consoler boxer.c -L../consoler -lconsoler -DPACKAGE_VERSION="$::VERSION"


if $::CHECK {
    execv gcc -o boxertest -I../consoler boxertest.c -L../consoler -lconsoler
    execv ./boxertest
}

execv a2x -v -f manpage -a VERSION=$::VERSION boxer.1.txt

if $::INSTALL {
    install -t $::PREFIX/bin boxer
    install -t $::PREFIX/share/man/man1 boxer.1
}

