#lang rosette

(require "../../lang.rkt"
         "vec-add-baseline.rkt"
         "vec-add.rkt")

(define SIZEX 3)
(define SIZE (* SIZEX SIZEX))
(define A (make-array (for/vector ([i SIZE]) (make-element i)) SIZE))
(define B (make-array (for/vector ([i SIZE]) (make-element i)) SIZE))
(define C (make-array (for/vector ([i SIZE]) (make-element 0)) SIZE))

(vec-add-baseline A B C SIZEX)
;(print-matrix C SIZEX SIZEX)

(invoke-kernel vec-add '(1 1) '(3 3) A B C SIZEX)
;(print-matrix C SIZEX SIZEX)

