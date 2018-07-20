#lang rosette

(require "../../lang.rkt"
         "convo.rkt")

(define SIZEX 6)
(define SIZE (* SIZEX SIZEX))
(define KERNEL-RADIUS 2)

(define in-data (make-array (for/vector ([i SIZE]) (make-element i)) SIZE))
(define out-data (make-array (for/vector ([i SIZE]) (make-element 0)) SIZE))
(define d-kernel (make-array (for/vector
                                 ([i (+ (* KERNEL-RADIUS 2) 1)])
                               (make-element 1))
                             (+ (* KERNEL-RADIUS 2) 1)))

(define out-file (open-output-file "profile.rkt" #:exists 'truncate))
(invoke-kernel-synth convo '(2 2) '(3 3) out-data in-data d-kernel SIZEX SIZEX out-file)
(print-matrix in-data SIZEX SIZEX)
(print-matrix out-data SIZEX SIZEX)
(close-output-port out-file)