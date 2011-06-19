
# Copyright (C) 2011 Richard Uhler
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

set mode [lindex $argv 0]
set src [lindex $argv 1]

# Format a block of text.
# Replaces all whitespace with a single space.
# Trims space off front and back. 
# Introduces newlines so as not to exceed 78 columns, and prefixes each line
# with the given indent.
proc formatblock {text indent} {
    set trimmed [concat $text]
    set formatted ""

    set cc [string length $indent]
    puts -nonewline "$indent"
    foreach {word} $trimmed { 
        incr cc [string length "$word "]
        if {$cc > 78} {
            set formatted "$formatted\n$indent"
            set cc [string length "$word "]
        }

        set formatted "$formatted$word "
    }
    return $formatted
}

# function which does nothing
proc nop {args} {
}

# Return the specialization function for the given function
# If there is none, returns a function which does nothing.
# Consults the mode variable to find the right function.
proc specialize {func} {
    global mode
    if {[string equal "" [info procs ${func}_$mode]]} {
        return nop
    }
    return ${func}_$mode
}

# docl command: section
# section name content
# Describes a new section in the document.
#  name - the name of the section 
#  content - a docl script containing the section content.
# For specialization:
#   ondocument_enter title - called on start of document with title.
#   ondocument_exit - called when document finishes
#   onsection_enter number title - called on start of a section.
#       number - section number, such as: "3.1.4"
#       title - section title
proc section {name content} {
    global istop
    global section_prefix
    global section_number
    set tl $istop
    set sp $section_prefix
    set sn $section_number

    if {$tl} {
        [specialize ondocument_enter] $name
    } else {
        [specialize onsection_enter] "$sp$sn" $name
        set section_prefix "$sp$sn."
    }

    set istop no
    set section_number 1
    eval $content

    set istop $tl
    set section_prefix $sp
    set section_number [expr $sn + 1]

    if {$istop} {
        [specialize ondocument_exit]
    }
}

# docl command: paragraph
# paragraph text
# Describes a paragraph of text.
#  text - the text of the paragraph.
# All whitespace is reduced to a single space and trimmed at front and back.
# For specialization:
#   onparagraph is called with formated text as its argument.
proc paragraph {text} {
    [specialize onparagraph] [formatblock $text ""]
}

# docl command: description
# description content
# Describes a name value list.
#  content - a list of elements alternating name value
# All whitespace in the values is reduced to a single space and trimmed at
# front and back.
# For specialization:
#   ondescription_enter - called when entering a description block.
#   ondescription_item name vlaue - called for each description item
#   ondescription_exit - called when exiting a description block.
proc description {content} {
    [specialize ondescription_enter]
    foreach {name value} $content {
        [specialize ondescription_item] $name $value
    }
    [specialize ondescription_exit]
}

# docl command: function
# function returntype name args description
# Describes a function.
#  returntype - the return type of the function
#  name - the name of the function
#  args - a list of elements alternating type name describing args to the
#         function.
#  description - a docl script describing the function (which should not
#                contain any section commands).
# For specialization:
#   onfunction_enter rtype name args
#   onfunction_exit
proc function {rtype name args description} {
    [specialize onfunction_enter] $rtype $name $args
    eval "$description"
    [specialize onfunction_exit]
}

# docl command: synopsis
# synopsis text
# Describe a program synopsis (usage)
#  text - the literal text of the synopsis
# For specialization:
#   (none)
proc synopsis {text} {
    puts "    $text"
    puts ""
}

# docl command: include
# include src
# Include content from the docl script in the file src relative to the current
# docl script.
#  src - name of source file to include
# For specialization:
#   (none)
proc include {src} {
    global srcdir
    set sd $srcdir
    set srcname [file join $sd $src]
    set srcdir [file dirname $srcname]
    source $srcname
    set srcdir $sd
}

# onparagraph_text
# The text specialization of onparagraph.
# Just outputs the pretty block of text.
proc onparagraph_text {text} {
    puts $text
    puts ""
}

proc ondescription_item_text {name value} {
    puts "    $name"
    puts [formatblock $value "        "]
    puts ""
}

proc ondocument_enter_text {title} {
    puts $title
    puts ""
}

proc onsection_enter_text {number title} {
    puts "$number $title"
    puts ""
}

proc onfunction_enter_text {rtype name args} {
    puts -nonewline "    $rtype $name\("
    set comma ""
    foreach {t n} $args {
        puts -nonewline "$comma$t $n"
        set comma ", "
    }
    puts ");\n"
}

# onparagraph_html
# The html specialization of onparagraph.
# Output the block between <p> </p> tags.
proc onparagraph_html {text} {
    puts "<p>"
    puts $text
    puts "</p>"
    puts ""
}

proc ondescription_enter_html {} {
    puts "<dl>"
}

proc ondescription_item_html {name value} {
    puts "    <dt>$name</dt>"
    puts "    <dd>"
    puts [formatblock $value "        "]
    puts "    </dd>"
    puts ""
}

proc ondescription_exit_html {} {
    puts "</dl>"
    puts ""
}

proc ondocument_enter_html {title} {
    puts "<html>"
    puts "  <title>$title</title>"
    puts "<body>"
    puts "<h1>$title</h1>"
    puts ""
}

proc ondocument_exit_html {} {
    puts "</body>"
    puts "</html>"
    puts ""
}

proc onsection_enter_html {number title} {
    set hl 2
    for {set i 0} {$i < [string length $number]} {incr i} {
        if {[string equal "." [string index $number $i]]} {
            incr hl
        }
    }

    puts "<h$hl>$number $title</h$hl>"
    puts ""
}

proc onfunction_enter_html {rtype name args} {
    puts -nonewline "    $rtype $name\("
    set comma ""
    foreach {t n} $args {
        puts -nonewline "$comma$t $n"
        set comma ", "
    }
    puts ");\n"
}

proc onparagraph_tex {text} {
    puts $text
    puts ""
}

proc ondescription_enter_tex {} {
    puts stdout "\\begin{description}"
}

proc ondescription_item_tex {name value} {
    puts "\\item \[$name\]"
    puts [formatblock $value "        "]
}

proc ondescription_exit_tex {} {
    puts "\\end{description}"
    puts ""
}

proc ondocument_enter_tex {title} {
    puts "\\documentclass{article}"
    puts "\\title{$title}"
    puts "\\begin{document}"
    puts "\\section{Introduction}"
    puts ""
}

proc ondocument_exit_tex {} {
    puts "\\end{document}"
    puts ""
}

proc onsection_enter_tex {number title} {
    puts "\\section{$title}"
    puts ""
}

proc onfunction_enter_tex {rtype name args} {
    puts "\\begin{verbatim}"
    puts -nonewline "    $rtype $name\("
    set comma ""
    foreach {t n} $args {
        puts -nonewline "$comma$t $n"
        set comma ", "
    }
    puts ");\n"
    puts "\\end{verbatim}"
}

proc onparagraph_man {text} {
    puts ".P"
    puts $text
}

proc ondescription_enter_man {} {
    puts ".P"
    puts ".RS"
    puts ".PD 0"
}

proc ondescription_item_man {name value} {
    puts ".TP"
    puts ".B $name"
    puts [formatblock $value ""] 
}

proc ondescription_exit_man {} {
    puts ".RE"
    puts ".PD"
}

proc ondocument_enter_man {title} {
    set date [clock format [clock seconds] -format "%Y-%m-%d"]
    puts ".TH [string toupper $title] 1 $date \"\" \"Focus Manual\""
}

proc onsection_enter_man {number title} {
    set depth 0
    for {set i 0} {$i < [string length $number]} {incr i} {
        if {[string equal "." [string index $number $i]]} {
            incr depth
        }
    }

    if {$depth == 0} {
        puts ".SH $title"
    } else {
        puts ".SS $title"
    }
}

proc emit {text} {
    global outfile
    if {![string equal "" $outfile]} {
        puts -nonewline $outfile $text
    }
}

proc emitl {text} {
    emit "$text\n"
}

proc onparagraph_libman {text} {
    emitl ".P"
    emitl $text
}

proc ondescription_enter_libman {} {
    emitl ".P"
    emitl ".RS"
    emitl ".PD 0"
}

proc ondescription_item_libman {name value} {
    emitl ".TP"
    emitl ".B $name"
    emitl [formatblock $value ""] 
}

proc ondescription_exit_libman {} {
    emitl ".RE"
    emitl ".PD"
}

proc onfunction_enter_libman {rtype name args} {
    global outfile
    global srcdir
    set outfile [open [file join $srcdir "$name.3"] "w"] 
    
    set date [clock format [clock seconds] -format "%Y-%m-%d"]
    emitl ".TH $name 3 $date \"\" \"Focus Manual\""
    emitl ".SH SYNOPSIS"
    emit ".RB $rtype $name ("
    set comma ""
    foreach {t n} $args {
        emit "$comma$t $n"
        set comma ", "
    }
    emitl ");"

    emitl ".SH DESCRIPTION"
}

proc onfunction_exit_libman {} {
    global outfile
    close $outfile
    set outfile ""
}

# global variables used in traversal
set istop yes
set section_prefix ""
set section_number 1
set srcdir [file dirname $src]
set outfile ""

# do the traversal
source $src

