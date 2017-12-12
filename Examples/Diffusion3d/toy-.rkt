#lang rosette

(require "../../lang.rkt" "toy.rkt")

(define out (open-output-file "res.rkt"
                              #:exists 'truncate))
(fprintf out "#lang rosette\n")
(writeln '(require "../../lang.rkt" "toy.rkt") out)
(writeln '(define switch #t) out)
(writeln (list 'define 'res-f
               (quasiquote
                (unquote (append (list 'lambda)
                                 (list (cdr (function res)))
                                 (body res)))))
         out)
(writeln '(for-each (lambda (e)
                      (displayln e)
                      23)
                    (optimize-barrier (spec-rotate-opt res-f))) out)
(close-output-port out)

(define path-to-racket-exe
  "/Applications/Racket v6.6/bin//racket")
(define path-to-rosette-code
  "/Users/akira/Masuhara Lab/Kani-CUDA/Examples/Diffusion3d/res.rkt")

(system*
 path-to-racket-exe
 path-to-rosette-code)
