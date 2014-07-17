
set ::titlebar "============================================================"

set ::mantemplate {
// Copyright (C) 2011 Richard Uhler <ruhler@member.fsf.org>
//
// This file is part of Focus.
//
// Focus is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.
//
// Focus is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with Focus.  If not, see <http://www.gnu.org/licenses/>.
@NAME@(3)
@TITLEBAR@===
:author: Richard Uhler
:doctype: manpage
:man version: @VERSION@
:man manual: Focus Manual

NAME
----
@NAME@ - @BRIEF@

SYNOPSIS
--------
@PROTOTYPE@;

DESCRIPTION
-----------
@DESCRIPTION@

BUGS
----
Report bugs to <ruhler@member.fsf.org>.
}

set ::doctemplate {
// Copyright (C) 2011 Richard Uhler <ruhler@member.fsf.org>
//
// This file is part of Focus.
//
// Focus is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.
//
// Focus is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with Focus.  If not, see <http://www.gnu.org/licenses/>.

    @PROTOTYPE@;

@DESCRIPTION@
}

proc librarydoc {fname} {
    set fin [open $fname r]
    set text [read $fin]
    set result [regexp -all -inline {/// ([a-zA-Z_0-9]+) - ([^\n]*)((?:\n///[^\n]*)*)\n([^;]*);} $text]

    foreach {full name brief desc_ proto} $result {
        set desc [string trim [string map {"\n///" "\n"} [string map {"\n/// " "\n"} $desc_]]]
        set bar [string range $::titlebar 1 [string length $name]]
        set mapping [list @NAME@ $name @TITLEBAR@ $bar @BRIEF@ $brief @DESCRIPTION@ $desc @PROTOTYPE@ $proto]

        set fout [open "$name.3.txt" w]
        puts $fout [string map $mapping $::mantemplate]
        close $fout

        set fout [open "$name.txt" w]
        puts $fout [string map $mapping $::doctemplate]
        close $fout
    }

    execv a2x -v -f manpage $name.3.txt
    if $::INSTALL {
        install -t $::PREFIX/share/man/man3 "$name.3"
    }
}

