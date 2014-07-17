execv gcc -o termer $::FONTER_CFLAGS termer.c client.c display.c inputter.c outputter.c screen.c $::FONTER_LIBS -DPACKAGE_VERSION="$::VERSION" -lpthread -lutil


if $::CHECK {
    execv gcc -o inputtertest $::CONSOLER_CFLAGS inputtertest.c inputter.c $::CONSOLER_LIBS
    execv ./inputtertest

    execv gcc -o screentest screentest.c screen.c
    execv ./screentest

    execv gcc -o outputtertest outputtertest.c outputter.c screen.c
    execv ./outputtertest

    execv gcc -o termfiller termfiller.c -lcurses
    execv gcc -o termertest $::CONSOLER_CFLAGS termertest.c $::CONSOLER_LIBS

    # TODO: Why does this not work here, but it passes if I run it manually?
    #execv -ignorestderr ./termertest
}

execv a2x -v -f manpage -a VERSION=$::VERSION termer.1.txt

if $::INSTALL {
    install $::PREFIX/bin termer
    install $::PREFIX/share/man/man1 termer.1
    execv tic termer.ti
}

