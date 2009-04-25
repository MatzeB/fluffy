#!/bin/bash

TEMPOUT="/tmp/fluffytest.txt"
for i in *.fluffy; do
	echo -n "$i..."
	../fluffy $i
	./a.out >& "$TEMPOUT" || echo "(return status != 0)"
	if ! diff "$TEMPOUT" "expected_output/$i.output" > /dev/null; then
		echo "FAIL"
	else
		echo ""
	fi
done

for i in shouldfail/*.fluffy; do
	echo -n "$i..."
	../fluffy $i >& /dev/null && echo -n "COMPILED (this is bad)"
	echo ""
done
rm -f a.out
