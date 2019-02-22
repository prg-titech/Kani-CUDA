#!/bin/bash

racket main.rkt $1
java -classpath ~/masuhara-lab/Kani-CUDA/Solver/kani-cuda/bin Expression.Main


