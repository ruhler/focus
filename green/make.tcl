
cmd::prog sgreen gcc -o sgreen $::CONSOLER_CFLAGS sgreen.c green.c green.h $::CONSOLER_LIBS -DPACKAGE_VERSION="$::VERSION" -lpthread
cmd::prog cgreen gcc -o cgreen $::CONSOLER_CFLAGS cgreen.c $::CONSOLER_LIBS -DPACKAGE_VERSION="$::VERSION"

cmd::man1 sgreen
cmd::man1 cgreen

make::check {
    execv gcc -o sgreentest $::CONSOLER_CFLAGS sgreentest.c $::CONSOLER_LIBS
    execv -ignorestderr ./sgreentest ./sgreen ../filler/filler

    execv gcc -o cgreentest $::CONSOLER_CFLAGS cgreentest.c $::CONSOLER_LIBS
    execv -ignorestderr ./cgreentest ./sgreen ./cgreen ../filler/filler
}


make::install cmd::install $::PREFIX/bin rgreen
make::clean execv rm -f cgreentest sgreentest

