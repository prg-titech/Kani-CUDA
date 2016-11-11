#lang rosette

(require "operators.rkt" "work.rkt")

(provide if/LS ?:/LS while/LS while-with-bound/LS)

;; denotation of if (b) {then-cl} {else-cl}
;; execute each clause with additional masks by b
;; then-cl, else-cl :: M -> ()
;; b :: () -> (vector? boolean?)
(define (if/LS b then-cl else-cl)
  (let ([bval (b)])
    (parameterize ([mask (&&/LS bval (mask))]) (then-cl))
    (parameterize ([mask (&&/LS (!/LS bval) (mask))]) (else-cl))))

(define (?:/LS b then-ex else-ex)
  (for/vector ([bval b]
               [m (mask)]
               [then then-ex]
               [else else-ex])
    (cond [(not m) 'masked-value]
          [bval then]
          [else else])))

;; denotation of while (b) {body}
;; execute body with addtional mask by b until all threads are masked
;; b :: () -> boolean? 
;; body :: M -> ()
(define (while/LS b body)
  (let* ([bval (b)]
         [m (&&/LS bval (mask))])
    (when (for/or ([v m]) v) ;; check that whether there are any non-masked thread
      (parameterize ([mask m])
        (body)
        (while/LS b body)))))

(define (while-with-bound/LS b body bound)
  (let* ([bval (b)]
         [m (&&/LS bval (mask))])
    (assert (> bound 0))
    (when (for/or ([v m]) v) ;; check that whether there are any non-masked thread
      (parameterize ([mask m])
        (body)
        (while-with-bound/LS b body (sub1 bound))))))

