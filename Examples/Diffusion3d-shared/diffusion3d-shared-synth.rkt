#lang rosette

(require "../../lang.rkt")

(current-bitwidth #f)

(synth-with-kani-cuda
 "diffusion3d-shared-sketch.rkt"
 "diffusion3d-shared-spec.rkt"
 "diffusion3d-shared-test.rkt")
