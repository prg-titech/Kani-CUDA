#lang rosette

(require rosette/lib/synthax)

(define bvsize 8)

(define-synthax (bit-shift x depth)
  #:base
  ((choose bvnot bvshl bvadd) (bv x bvsize) (?? (bitvector bvsize)))
  #:else
  ((choose bvnot bvshl bvadd) (bit-shift x (- depth 1)) (?? (bitvector bvsize))))

(define (sample i)
  (if (>= i 0)
      (bv (* 2 i) bvsize)
      (bv (* -2 i) bvsize)))

(define (sample-sketch i)
  (if (>= i 0)
      (bit-shift i 3)
      (bit-shift i 3)))

(define-symbolic i integer?)

(generate-forms
 (synthesize
  #:forall (list i)
  ;  #:assume (assert (&& (< i 10) (< -10 i)))
  #:guarantee (begin
                (for ([i 2])
                  (assert (bveq (sample i) (sample-sketch i)))
                  (assert (bveq (sample (* -1 i)) (sample-sketch (* -1 i))))))))