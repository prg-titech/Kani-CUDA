#lang rosette

(require "operators.rkt" "work.rkt")

(provide if/LS while/LS)

;; denotation of if (b) {then-cl} {else-cl}
;; execute each clause with additional masks by b
;; then-cl, else-cl :: M -> ()
;; b :: () -> (vector? boolean?)
(define (if/LS b then-cl else-cl)
  (let ([bval (b)])
    (parameterize ([mask (&&/LS bval (mask))]) (then-cl))
    (parameterize ([mask (&&/LS (!/LS bval) (mask))]) (else-cl))))

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