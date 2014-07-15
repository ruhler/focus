
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

