
make::all execv gcc -o sdlcsr $::CONSOLER_CFLAGS $::SDL_CFLAGS sdlcsr.c $::CONSOLER_LIBS $::SDL_LIBS -DPACKAGE_VERSION="$::VERSION"
make::install cmd::install $::PREFIX/bin sdlcsr
cmd::man1 sdlcsr
make::clean execv rm -f sdlcsr

