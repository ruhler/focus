
execv gcc -o boxer $::CONSOLER_CFLAGS boxer.c $::CONSOLER_LIBS -DPACKAGE_VERSION="$::VERSION"


if $::CHECK {
    execv gcc -o boxertest $::CONSOLER_CFLAGS boxertest.c $::CONSOLER_LIBS
    execv ./boxertest
}

execv a2x -v -f manpage -a VERSION=$::VERSION boxer.1.txt

if $::INSTALL {
    install -t $::PREFIX/bin boxer
    install -t $::PREFIX/share/man/man1 boxer.1
}

