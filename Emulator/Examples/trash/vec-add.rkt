#lang rosette

(require "../../lang.rkt")

(define SIZE 10)

(define (i)
  (define-symbolic* i integer?)
  i)

(define A (make-array (for/vector ([j SIZE]) (make-element (i))) SIZE))
(define A- (make-array (for/vector ([j SIZE]) (make-element j)) SIZE))
(define B (make-array (for/vector ([j SIZE]) (make-element (i))) SIZE))

(define (check len A B)
  (for ([i len])
    (assert (eq? (array-ref-host A i) (array-ref-host B i)))))

;; kernel
(define (vec-add C A B)
  (:= int i (thread-idx 0))
  (:shared int smem[(block-size)])
  (= [smem i] [A i])
  (= [C i] (+/LS (profiling-access A i) [B i])))

;; host
(define (host len A B)
  (define C (make-array (for/vector ([i SIZE]) (make-element 0)) len))
  (invoke-kernel vec-add '(1) (list len) C A B)
  C)

;; specification
(define (spec len A B)
  (define C (make-array (for/vector ([i SIZE]) (make-element 0)) len))
  (for ([i len])
    (array-set-host! C i (+ (array-ref-host A i) (array-ref-host B i))))
  C)

(define lstA
  (for/list ([i SIZE])
    (array-ref-host A i)))

(define lstB
  (for/list ([i SIZE])
    (array-ref-host B i)))

(define lst (append lstA lstB))

(define (synth)
  (define-values (_ cpu real gc)
    (time-apply
     (lambda () 
       (synthesize #:forall lst
                   #:guarantee (check SIZE (host SIZE A B) (spec SIZE A B))))
     '()))
  (list cpu real gc))

;(synth)
(host SIZE A- B)

;(map syntax->datum (generate-forms (synth)))





