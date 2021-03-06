
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

    # man1 foo
    #  Builds a man page foo.1 from an asciidoc file foo.1.txt.
    #  The asciidoc file may refer to the variable VERSION with the focus
    #  version number.
    #
    #  Arranges for said man page to be installed in the right place.
    proc man1 {x} {
        make::all execv a2x -v -f manpage -a VERSION=$::VERSION $x.1.txt
        make::install cmd::install $::PREFIX/share/man/man1 $x.1
        make::clean execv rm -f $x.1
    }

    # pkgconfig foo
    # Builds foo.pc from a template foo.pc.in by substituting the values of
    #   @PACKAGE_VERSION@ and @prefix@ in foo.pc.in.
    # Arranges for said pkgconfig file to be installed in the right place.
    proc pkgconfig {x} {
        make::all execv sed -e s=@PACKAGE_VERSION@=$::VERSION= -e s=@prefix@=$::PREFIX= $x.pc.in > $x.pc
        make::install cmd::install $::PREFIX/lib/pkgconfig $x.pc
        make::clean execv rm -f $x.pc
    }

    # Build a program of the given name using the given command.
    # Arranges for the program to be installed in the bin directory.
    proc prog {name cmd} {
        make::all execv $cmd
        make::install cmd::install $::PREFIX/bin $name
        make::clean execv rm -f $name
    }

    # Build a unit test program with the given name and run it as a test case.
    #   name - name of generated test program
    #   build - command to build the test program
    #   run - command to run the test program
    proc test {name build run} {
        make::check execv $build
        make::check execv -ignorestderr $run
        make::clean execv rm -f $name
    }

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

