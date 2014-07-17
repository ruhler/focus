
# Explicitly initialize the environment.
foreach key [array names ::env] {
    array unset ::env $key
}
set ::env(PATH) "/usr/bin:/bin"
set ::env(LANG) "en_US.UTF-8"

set ::PREFIX "/home/ruhler/local"
set ::VERSION "1.2.0"
set ::CHECK yes
set ::INSTALL no
set ::FREETYPE_CFLAGS -I/usr/include/freetype2
set ::FONTCONFIG_CFLAGS -I/usr/include/freetype2
set ::FREETYPE_LIBS -lfreetype
set ::FONTCONFIG_LIBS "-lfontconfig -lfreetype"
set ::POPPLER_CFLAGS "-I/home/ruhler/local/include/poppler/cpp -I/home/ruhler/local/include/poppler -I/usr/include/cairo -I/usr/include/glib-2.0 -I/usr/lib/x86_64-linux-gnu/glib-2.0/include -I/usr/include/pixman-1 -I/usr/include/freetype2 -I/usr/include/libdrm -I/usr/include/libpng12"
set ::POPPLER_LIBS "-L/home/ruhler/local/lib -lpoppler-cpp -lpoppler-cairo -lm -lstdc++ -pthread -lpoppler -lcairo"
set ::SDL_CFLAGS "-D_GNU_SOURCE=1 -D_REENTRANT -I/usr/include/SDL"
set ::SDL_LIBS "-lSDL"

set ::BUILD_ROOT [pwd]

rename exec tclexec

proc exec {args} {
    puts "$args"
    eval tclexec $args
}

proc execv {args} {
    exec {*}[join $args]
}

proc indir {dir script} {
    set wd [pwd]
    puts "tclmk: Entering directory `$wd/$dir'"
    cd $dir
    eval $script
    puts "tclmk: Leaving directory `$wd/$dir'"
    cd $wd
}

