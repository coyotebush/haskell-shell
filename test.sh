#!/bin/sh

EXE1='bash --norc --noediting -i'
EXE2='./sh.hs'
TESTS='
001prompt
002multiprompt
'
DIFF='diff -U3'

set -e
for t in $TESTS
do
    echo Running test case $t
    PS1='$ ' $EXE1 < tests/$t.in > tests/output.1 2>&1
    $EXE2 < tests/$t.in > tests/output.2
    $DIFF tests/output.1 tests/output.2
done
