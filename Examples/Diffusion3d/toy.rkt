#lang rosette

(require "../../lang.rkt")
(require rosette/lib/synthax)

(current-bitwidth #f)

(define-synthax (arith-exp a b depth)
  #:base (choose a b ((choose + - * /) (choose a b) (??)))
  #:else ((choose + - * /)
          (arith-exp a b (- depth 1))
          (arith-exp a b (- depth 1))))

(define-synthax (bool-exp i j tid-x tid-y c2 bx by nx ny nz depth)
  #:base (choose 0 i j tid-x tid-y c2 bx by nx ny nz
                 (- (choose i j tid-x tid-y c2 bx by nx ny nz) 1))
  #:else (choose
          ((choose && || eq?)
           (bool-exp i j tid-x tid-y c2 bx by nx ny nz (- depth 1))
           (bool-exp i j tid-x tid-y c2 bx by nx ny nz (- depth 1)))
          (not (bool-exp i j tid-x tid-y c2 bx by nx ny nz (- depth 1)))))

(define (bool-exp- i j tid-x tid-y c2 bx by nx ny nz)
  (bool-exp i j tid-x tid-y c2 bx by nx ny nz 3))

(define (bool-exp-- i j tid-x tid-y c2 bx by nx ny nz)
  (&& (eq? tid-x 0) (not (eq? i (choose 1 0)))))

(define (add- x y)
  (choose (arith-exp x y 1)
          (arith-exp x y 2)
          (arith-exp x y 3)))

(define (synth)
  (map syntax->datum
       (generate-forms
        (time
         (synthesize #:forall '()
                     #:guarantee (begin
                                   (assert (eq? (add- 1 2) 3))
                                   (assert (eq? (add- 2 2) 4))
                                   (assert (eq? (add- 3 2) 5))
                                   (assert (eq? (add- 1 8) 9))
                                   (assert (eq? (add- 5 8) 13))
                                   (assert (eq? (add- 9 6) 15))
                                   (assert (eq? (add- 9 1) 10))))))))



(map syntax->datum
     (generate-forms
      (time
       (synthesize #:forall '()
                  #:guarantee
                  (begin
                    (define in (open-input-file "profile.rkt"))
                    (define stmt "")
                    (set! stmt (read-line in))
                    (while (not (eof-object? stmt))
                    ;(for ([i 60])
                      (let ([lst (map string->number (string-split stmt " "))])
                        ;(println lst)
                        (if (list-ref lst 3)
                            (assert (not (bool-exp- (list-ref lst 4) (list-ref lst 5) (list-ref lst 6) (list-ref lst 7) (list-ref lst 8)
                                                    (list-ref lst 9) (list-ref lst 10) (list-ref lst 11) (list-ref lst 12) (list-ref lst 13))))
                            (assert (bool-exp- (list-ref lst 4) (list-ref lst 5) (list-ref lst 6) (list-ref lst 7) (list-ref lst 8)
                                               (list-ref lst 9) (list-ref lst 10) (list-ref lst 11) (list-ref lst 12) (list-ref lst 13))))
                        (set! stmt (read-line in))))
                    (close-input-port in))))))
