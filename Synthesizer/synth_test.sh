#!/bin/bash

racket out.rkt
echo Synthesizing...
java -classpath ../Solver/kani-cuda/bin Expression.Main
