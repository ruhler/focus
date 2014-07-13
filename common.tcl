
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

rename exec tclexec

proc exec {args} {
    puts "$args"
    tclexec {*}$args
}

proc indir {dir script} {
    set wd [pwd]
    puts "tclmk: Entering directory `$wd/$dir'"
    cd $dir
    eval $script
    puts "tclmk: Leaving directory `$wd/$dir'"
    cd $wd
}

