
set sourcefile [lindex $argv 0]

# print a formated block of text.
# Replaces all whitespace with a single space.
# Trims space off front and back. 
# Introduces newlines so as not to exceed 78 columns, and prefixes each line
# with the given indent.
# puts a trailing blank line after the block of text.
proc textblock {text indent} {
    set trimmed [concat $text]

    set cc [string length $indent]
    puts -nonewline "$indent"
    foreach {word} $trimmed { 
        incr cc [string length "$word "]
        if {$cc > 78} {
            puts ""
            set cc [string length "$word "]
            puts -nonewline "$indent"
        }

        puts -nonewline "$word "
    }
    puts "\n"
}

# Generate on standard out a text document from the input .docl source.
proc gentext {src} {
    global gentext_toplevel
    global gentext_section_prefix
    global gentext_section_number
    set gentext_toplevel yes
    set gentext_section_prefix ""
    set gentext_section_number 1
    

    proc section {name content} {
        global gentext_toplevel
        global gentext_section_prefix
        global gentext_section_number
        set tl $gentext_toplevel
        set sp $gentext_section_prefix
        set sn $gentext_section_number

        if {$tl} {
            puts "$name"
        } else {
            puts "$sp$sn $name"
            set gentext_section_prefix "$sp$sn."
        }
        puts ""

        set gentext_toplevel no
        set gentext_section_number 1
        eval $content

        set gentext_toplevel $tl
        set gentext_section_prefix $sp
        set gentext_section_number [expr $sn + 1]
    }

    proc paragraph {content} {
        textblock $content ""
    }

    proc description {content} {
        foreach {key value} $content {
            puts "    $key"
            textblock $value "        "
        }
        puts ""
    }

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

    source $src
}

gentext $sourcefile

