
make::all execv gcc -o filler $::CONSOLER_CFLAGS main.c filler.c eventer.c $::CONSOLER_LIBS -DPACKAGE_VERSION="$::VERSION" -ltcl
cmd::man1 filler

make::check {
    execv gcc -o fillertest $::CONSOLER_CFLAGS fillertest.c $::CONSOLER_LIBS
    execv -ignorestderr ./fillertest
}


make::install cmd::install $::PREFIX/bin filler
make::clean execv rm -f filler fillertest

