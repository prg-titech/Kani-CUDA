#lang rosette
 
(require "../../lang.rkt"
         "mulmat-baseline.rkt"
         "mulmat.rkt")

(define SIZEX 12)
(define SIZE (* SIZEX SIZEX))
(define A (make-array (for/vector ([i SIZE]) (make-element i)) SIZE))
(define B (make-array (for/vector ([i SIZE]) (make-element i)) SIZE))
(define C (make-array (for/vector ([i SIZE]) (make-element 0)) SIZE))

(mulmat-baseline A B C SIZEX)
(print-matrix C SIZEX SIZEX)


(define out-file (open-output-file "profile.rkt" #:exists 'truncate))
(invoke-kernel-synth mulmat '(3 3) '(4 4) A B C SIZEX out-file)
(print-matrix C SIZEX SIZEX)
(close-output-port out-file)

