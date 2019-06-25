#!/bin/bash

racket main.rkt $1 || {
    c=$?;
    echo "Converting $1 to KaniCUDA failed.";
    exit $c;
}
racket out.rkt || {
    c=$?;
    echo "Converting $1 to KaniCUDA failed.";
    exit $c;
}
echo Synthesizing...
java -classpath ../Solver/kani-cuda/bin Expression.Main
