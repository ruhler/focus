
cmd::prog filler { gcc -o filler $::CONSOLER_CFLAGS main.c filler.c eventer.c $::CONSOLER_LIBS -DPACKAGE_VERSION="$::VERSION" -ltcl }
cmd::man1 filler
cmd::test fillertest {gcc -o fillertest $::CONSOLER_CFLAGS fillertest.c $::CONSOLER_LIBS } ./fillertest

