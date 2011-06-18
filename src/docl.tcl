
set src [lindex $argv 0]

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
    return "$formatted\n"
}
    

# docl command: section
# section name content
# Describes a new section in the document.
#  name - the name of the section 
#  content - a docl script containing the section content.
proc section {name content} {
    global istop
    global section_prefix
    global section_number
    set tl $istop
    set sp $section_prefix
    set sn $section_number

    if {$tl} {
        puts "$name"
    } else {
        puts "$sp$sn $name"
        set section_prefix "$sp$sn."
    }
    puts ""

    set istop no
    set section_number 1
    eval $content

    set istop $tl
    set section_prefix $sp
    set section_number [expr $sn + 1]
}

# docl command: paragraph
# paragraph text
# Describes a paragraph of text.
#  text - the text of the paragraph.
# All whitespace is reduced to a single space and trimmed at front and back.
proc paragraph {text} {
    puts [formatblock $text ""]
}

# docl command: description
# description content
# Describes a name value list.
#  content - a list of elements alternating name value
# All whitespace in the values is reduced to a single space and trimmed at
# front and back.
proc description {content} {
    foreach {name value} $content {
        puts "    $name"
        puts [formatblock $value "        "]
    }
    puts ""
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
proc function {rtype name args description} {
    puts -nonewline "    $rtype $name\("
    set comma ""
    foreach {t n} $args {
        puts -nonewline "$comma$t $n"
        set comma ", "
    }
    puts ");\n"
    eval "$description"
}

# docl command: synopsis
# synopsis text
# Describe a program synopsis (usage)
#  text - the literal text of the synopsis
proc synopsis {text} {
    puts "    $text"
    puts ""
}

# docl command: include
# include src
# Include content from the docl script in the file src relative to the current
# docl script.
#  src - name of source file to include
proc include {src} {
    global srcdir
    set sd $srcdir
    set srcname [file join $sd $src]
    set srcdir [file dirname $srcname]
    source $srcname
    set srcdir $sd
}

# global variables used in traversal
set istop yes
set section_prefix ""
set section_number 1
set srcdir [file dirname $src]

# do the traversal
source $src

