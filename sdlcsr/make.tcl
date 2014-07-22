
cmd::prog sdlcsr gcc -o sdlcsr $::CONSOLER_CFLAGS $::SDL_CFLAGS sdlcsr.c $::CONSOLER_LIBS $::SDL_LIBS -DPACKAGE_VERSION="$::VERSION"
cmd::man1 sdlcsr

