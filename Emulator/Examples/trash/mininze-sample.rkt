#lang rosette

(require rosette/lib/synthax)

(define-symbolic a b boolean?)

(define z3 (current-solver))

(define (sample)
  (define x 0)
  (define y 0)
  (set! x (choose 1 2))
  (set! y (choose 2 3 4))
  (+ x y))

(solver-minimize z3 (list (sample)))

(map syntax->datum (generate-forms (synthesize #:forall '()
                                               #:guarantee (sample))))

(map syntax->datum (generate-forms (solver-check z3)))


