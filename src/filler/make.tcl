
execv gcc -o filler $::CONSOLER_CFLAGS main.c filler.c eventer.c $::CONSOLER_LIBS -DPACKAGE_VERSION="$::VERSION" -ltcl

if $::CHECK {
    execv gcc -o fillertest $::CONSOLER_CFLAGS fillertest.c $::CONSOLER_LIBS
    execv -ignorestderr ./fillertest
}

execv a2x -v -f manpage -a VERSION=$::VERSION filler.1.txt

if $::INSTALL {
    install $::PREFIX/bin filler
    install $::PREFIX/share/man/man1 filler.1
}

