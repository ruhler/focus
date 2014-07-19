
rename exec tclexec

proc exec {args} {
    puts "$args"
    eval tclexec $args
}

proc execv {args} {
    exec {*}[join $args]
}

# Install to the directory 'dest' each of the files given.
namespace eval cmd {
    proc install {dest args} {
        foreach x $args {
            execv mkdir -p $dest
            execv cp $x $dest
        }
    }
}

proc indir {dir script} {
    set wd [pwd]
    puts "tclmk: Entering directory `$wd/$dir'"
    cd $dir
    eval $script
    puts "tclmk: Leaving directory `$wd/$dir'"
    cd $wd
}

namespace eval make {
    set targetidx [lsearch $argv "--target"]
    variable target "all"
    if {$targetidx >= 0 && $targetidx+1 < [llength $argv]} {
        set target [lindex $argv [expr $targetidx+1]]
    }

    proc all {args} {
        variable target
        if {[lsearch {all install} $target] != -1} { eval {*}$args }
    }

    proc install {args} {
        variable target
        if [string equal $target "install"] { eval {*}$args }
    }

    proc check {args} {
        variable target
        if [string equal $target "check"] { eval {*}$args }
    }

    proc clean {args} {
        variable target
        if [string equal $target "clean"] {eval {*}$args }
    }
}

