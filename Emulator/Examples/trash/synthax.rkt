#lang rosette

(require rosette/lib/synthax)

(current-bitwidth #f)

(define-synthax (formula- x y z depth)
  #:base (choose x y z)
  #:else (choose x y z
                 ((choose + - * /) (formula- x y z (- depth 1))
                                   (formula- x y z (- depth 1)))))

(define (formula x y z)
  (formula- x y z 3))

(define (synth-formula)
  (time (synthesize #:forall '()
                    #:guarantee (assert (eq? (formula- 1 2 3 0) 100)))))

(map syntax->datum
    (generate-forms (synth-formula)))



