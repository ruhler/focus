#!/bin/sh

export SHELL=`dirname $0`/benchshell.sh
./termer_prof > /dev/null
unset SHELL
