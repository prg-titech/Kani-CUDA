#lang rosette

(require
  (rename-in rosette/lib/synthax [choose ?]))

(define (add x y)
  ((? + - * /)  x y))

(generate-forms
 (synthesize #:forall '()
            #:guarantee (assert (eq? 3 (add 1 2)))))