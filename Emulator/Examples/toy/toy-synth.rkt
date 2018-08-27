#lang rosette

(require "../../lang.rkt")

(current-bitwidth #f)

(synth-with-kani-cuda
 "toy-sketch.rkt"
 "toy-spec.rkt"
 "toy-test.rkt")

