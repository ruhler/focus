
make::all {
    execv gcc -o boxer $::CONSOLER_CFLAGS boxer.c $::CONSOLER_LIBS -DPACKAGE_VERSION="$::VERSION"
    execv a2x -v -f manpage -a VERSION=$::VERSION boxer.1.txt
}

make::check {
    execv gcc -o boxertest $::CONSOLER_CFLAGS boxertest.c $::CONSOLER_LIBS
    execv ./boxertest
}


make::install {
    cmd::install $::PREFIX/bin boxer
    cmd::install $::PREFIX/share/man/man1 boxer.1
}

make::clean execv rm -f boxer boxer.1 boxertest

