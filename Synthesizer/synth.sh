#!/bin/bash

racket main.rkt $1
racket out.rkt
if [ -d profiles.old ]; then
	rm -r profiles.old
fi
mv profiles profiles.old
racket out.rkt
cd profiles.old
for p in __opt__*; do
	cat $p >> ../profiles/$p
done
cd ..
echo Synthesizing...
java -classpath ../Solver/kani-cuda/lib/commons-math3-3.0.jar:../Solver/kani-cuda/bin Expression.Main