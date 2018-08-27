#lang rosette

(require rosette/lib/synthax "../../lang.rkt")

(current-bitwidth #f)

(define-synthax (arith-exp BLOCKSIZE bdim-y tid-x tid-y c bdim-x depth)
  #:base (choose 0 1 2 BLOCKSIZE bdim-y tid-x tid-y c bdim-x)
  #:else (choose 0 1 2 BLOCKSIZE bdim-y tid-x tid-y c bdim-x ((choose + - *) (arith-exp BLOCKSIZE bdim-y tid-x tid-y c bdim-x (- depth 1)) (arith-exp BLOCKSIZE bdim-y tid-x tid-y c bdim-x (- depth 1)))))

(define (arith-exp- BLOCKSIZE bdim-y tid-x tid-y c bdim-x) (arith-exp BLOCKSIZE bdim-y tid-x tid-y c bdim-x 2))

(define (spec-arith-exp file)
  (define in (open-input-file file))
  (define stmt 0)
  (set! stmt (read-line in))
  (while (not (eof-object? stmt))
         (let ([lst (map string->number (string-split stmt " "))])
           (when (list-ref lst 3)
             (assert
              (eq? (list-ref lst 2)
                   (arith-exp- (list-ref lst 4) (list-ref lst 5) (list-ref lst 6) (list-ref lst 7) (list-ref lst 8) (list-ref lst 9)))))
           (set! stmt (read-line in))))
  (close-input-port in))
(list-ref (list-ref (map syntax->datum
       (generate-forms
        (time
         (synthesize #:forall '()
                     #:guarantee 
                     (spec-arith-exp "profile.rkt")
                     )))) 0) 2)