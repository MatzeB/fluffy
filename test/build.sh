#!/bin/sh

mlang test/testprog || exit 1
gcc -g3 out.s test/jodel.c -o testprog || exit 1

