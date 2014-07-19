
make::all {
    execv gcc -o filler $::CONSOLER_CFLAGS main.c filler.c eventer.c $::CONSOLER_LIBS -DPACKAGE_VERSION="$::VERSION" -ltcl
    execv a2x -v -f manpage -a VERSION=$::VERSION filler.1.txt
}

make::check {
    execv gcc -o fillertest $::CONSOLER_CFLAGS fillertest.c $::CONSOLER_LIBS
    execv -ignorestderr ./fillertest
}


make::install {
    cmd::install $::PREFIX/bin filler
    cmd::install $::PREFIX/share/man/man1 filler.1
}

make::clean execv rm -f filler filler.1 fillertest

