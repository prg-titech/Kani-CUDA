#!/bin/bash

racket main.rkt $1
racket out.rkt
echo Synthesizing...
java -classpath ../Solver/kani-cuda/bin Expression.Main
