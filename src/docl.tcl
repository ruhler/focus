
# Copyright (C) 2011 Richard Uhler <ruhler@member.fsf.org>
#
# This file is part of Focus.
#
# Focus is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# Focus is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with Focus.  If not, see <http://www.gnu.org/licenses/>.

set src [lindex $argv 0]

# function docl command
# function rtype name args description
#
# For each function we generate:
# $name.txt - an asciidoc file which contains the description with the first
#             four spaces of each line removed.
# $name.3.txt - an asciidoc file describing the man page
# These are generated relative to the sourced file.
proc function {rtype name args desc} {
    set dir [file dirname [info script]]

    set txtfile [open "$dir/$name.txt" "w"]
    puts $txtfile [string map {"\n    " "\n"} $desc]
    close $txtfile

    set titlebar ""
    while {[string length $titlebar] < [string length "$name\(3)"]} {
        set titlebar "=$titlebar"
    }

    set synopsis "$rtype $name\("
    set comma ""
    foreach {t n} $args {
        set synopsis "$synopsis$comma$t $n"
        set comma ", "
    }
    set synopsis "$synopsis);"

    set manfile [open "$dir/$name.3.txt" "w"]
    puts $manfile "$name\(3)
$titlebar
:author: Richard Uhler
:doctype: manpage
:man version: {VERSION}
:man manual: Focus Manual

NAME
----
$name - $name

SYNOPSIS
--------
$synopsis

DESCRIPTION
-----------
include::$name.txt\[]

BUGS
----
Report bugs to <ruhler@member.fsf.org>."
    close $manfile

}

source $src

