#lang rosette

(require "operators.rkt" "work.rkt")

(provide if/LS while/LS while-with-bound/LS for/LS)

;; Denotation of if (b) {then-cl} {else-cl}
;; Execute each clause with additional masks by b
;; then-cl, else-cl :: M -> ()
;; b :: () -> (vector? boolean?)
(define (if/LS b then-cl else-cl)
  (let ([bval (b)])
    (parameterize ([mask (&&/LS bval (mask))]) (then-cl))
    (parameterize ([mask (&&/LS (!/LS bval) (mask))]) (else-cl))))

;; Denotation of while (b) {body}
;; Execute body with addtional mask by b until all threads are masked
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


;; Denotation of for (init;cond;change) {body}
;; Execute body with addtional mask by b until all threads are masked
;; init :: M -> ()
;; cond :: () -> boolean?
;; incr :: M -> ()
(define (for/LS init cond incr body)
  (init)
  (while/LS
   cond
   (lambda ()
     (body)
     (incr))))

