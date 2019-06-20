#!/bin/bash

racket main.rkt $1
racket out.rkt
echo Synthesizing...
java -classpath ../Solver/kani-cuda/lib/commons-math3-3.0.jar:../Solver/kani-cuda/bin Expression.Main
