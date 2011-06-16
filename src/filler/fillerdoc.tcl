
set brief {Solid color screen filler}

set desc {filler is a consoler client which fills the screen with a solid
color. The color can be changed using appropriate key strokes. The primary
motivation of this application is to serve as a simple client for testing
consoler servers.}

set keyintro {filler initially fills the screen with black. You can then press
any of the following keys to fill the screen with the corresponding color.}

set keymap "r red g green b blue c cyan y yellow p purple w white n black"

set toquit {Use the key q to quit the application.}

set environ [list CNSLWIDTH {The width of the window to use.
Defaults to 640 pixels if not set.} CNSLHEIGHT {The
height of the window to use. Defaults to 480 pixels if not set.}]

# Print a man page for filler to standard out.
puts ".TH FILLER 1 2011-06-11 \"\" \"Focus Manual\"
.SH NAME
filler \\- $brief
.SH SYNOPSIS
.B filler
.SH DESCRIPTION
$desc
.P
$keyintro
.P
.RS
.PD 0"
foreach {k c} $keymap {
   puts ".TP\n.B $k\n$c"
}
puts ".RE\n.PD\n.P\n$toquit"
puts ".SH OPTIONS
None
.SH ENVIRONMENT"
foreach {e d} $environ {
   puts ".TP\n.B $e\n$d"
}
puts ".SH BUGS\nReport bugs to\n.BR \\| < ruhler@csail.mit.edu >."

