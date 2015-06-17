#!/bin/bash

mkdir -p temp
cd temp
flex -l ../scan.l
if [ $? -eq 0 ]; then
    echo flex succeeded
else
    echo flex failed
    exit 1
fi
bison -y -d ../gram.y
if [ $? -eq 0 ]; then
    echo bison succeeded
else
    echo bison failed
    exit 1
fi
gcc -pedantic y.tab.c lex.yy.c -o ../a.out
if [ $? -eq 0 ]; then
    echo gcc succeeded
else
    echo gcc failed
    exit 1
fi
