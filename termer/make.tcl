
cmd::prog termer { gcc -o termer $::FONTER_CFLAGS termer.c client.c display.c inputter.c outputter.c screen.c $::FONTER_LIBS -DPACKAGE_VERSION="$::VERSION" -lpthread -lutil }
cmd::man1 termer

cmd::test inputtertest { gcc -o inputtertest $::CONSOLER_CFLAGS inputtertest.c inputter.c $::CONSOLER_LIBS } ./inputtertest
cmd::test screentest { gcc -o screentest screentest.c screen.c } ./screentest
cmd::test outputtertest { gcc -o outputtertest outputtertest.c outputter.c screen.c } ./outputtertest
cmd::test termfiller { gcc -o termfiller termfiller.c -lcurses } true

# TODO: Why does this not work here, but it passes if I run it manually?
#cmd::test termertest { gcc -o termertest $::CONSOLER_CFLAGS termertest.c $::CONSOLER_LIBS } ./termertest

