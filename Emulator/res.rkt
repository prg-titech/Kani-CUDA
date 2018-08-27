#lang rosette
(require "lang.rkt")

(define res-f
  (lambda (arr SIZE)
    (:= int i (thread-idx 0))
    (if (switch) (? (syncthreads) (void)) (syncthreads))
    (:shared real smem ((block-size)))
    (if (switch) (? (syncthreads) (void)) (syncthreads))
    (= (smem i) (arr i))
    (if (switch) (? (syncthreads) (void)) (syncthreads))
    (:= int x (arr i))
    (if (switch) (? (syncthreads) (void)) (syncthreads))
    (= (arr (modulo/LS (+/LS i -94) SIZE)) x)))

(define (array-eq-assert arr1 arr2 len)
  (for
   ((i (in-range len)))
   (assert (eq? (array-ref-host arr1 i) (array-ref-host arr2 i)))))

(define (spec-opt res-f)
  (define SIZE 19)
  (define in0-opt (make-array (for/vector ((i SIZE)) (make-element i)) SIZE))
  (define in1-opt (make-array (for/vector ((i 5)) (make-element i)) 5))
  (define out0
    (make-array
     (for/vector ((i SIZE)) (make-element (modulo (+ i -1) SIZE)))
     SIZE))
  (define out1
    (make-array (for/vector ((i 5)) (make-element (modulo (+ i -1) 5))) 5))
  (invoke-kernel res-f '(1) (list SIZE) in0-opt SIZE)
  (invoke-kernel res-f '(1) '(5) in1-opt 5)
  (array-eq-assert in0-opt out0 SIZE)
  (array-eq-assert in1-opt out1 5))

(for-each
 (lambda (e) (pretty-display e))
 (optimize-barrier (parameterize ((switch #t)) (spec-opt res-f))))

