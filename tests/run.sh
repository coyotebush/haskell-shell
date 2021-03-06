#!/bin/bash

# http://stackoverflow.com/a/246128/230170 
TESTDIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"

EXE1="bash --rcfile $TESTDIR/bashrc --noediting -i"
EXE2='./sh'
DIFF='diff -U3'
MAKE='ghc --make sh.hs'

if [ ! -z "$MAKE" ]; then $MAKE || exit; fi

for t in tests/[0-9]*
do
    echo Running test case $t
    HISTFILE= $EXE1 < $t > $TESTDIR/output.1 2>&1
    $EXE2 < $t | sed -e 's/sh.hs/bash/' > $TESTDIR/output.2
    $DIFF $TESTDIR/output.1 $TESTDIR/output.2 || exit
done
