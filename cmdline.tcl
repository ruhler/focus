
# cmdline - Command line argument parser.
#
# A command line is a sequence of words. Each word is either a flag or an
# argument. Each flag has either no argument, an optional argument, or a
# required argument.
#
# A flag which has no argument is written standalone.
#  example: --foo
#
# A flag with an optional argument is written either standalone (to indicate
# there is no argument), or using an equals sign to denote the argument:
#  example: --foo
#  example: --foo=bar
# If the argument is optional, it must be affixed to the flag with '=', it may
# not appear as a standalone next argument. In particular, if "--foo" takes an
# optional argument, then:  --foo bar  is treated as --foo having no argument,
# and bar is the next flag.
#
# A flag with a required argument is written either as a standalone flag,
# where the next word is treated as the argument, or using an equals sign to
# denote the argument:
#  example: --foo bar
#  example: --foo=bar
#
# An options specification is a tcl procedure which takes a single argument:
# the next flag being parsed.
# The result of the options specification should be:
#   NoArg - to indicate the flag takes no argument
#   OptArg - to indicate the flag has an optional argument
#   ReqArg - to indicate the flag has a required argument
#   Invalid - to indicate the flag is invalid
#   ArgFor=<flag> - to indicate the word is an argument for the given flag.
#
proc cmdline {} {
}


