#lang rosette

(require "../../lang.rkt")

(provide (all-defined-out))

(current-bitwidth 10)

(define (r)
  (define-symbolic* r real?)
  r)

(define (array-eq-verify arr1 arr2 len)
  (for ([i (in-range len)])
    (assert
     (eq?
      (array-ref-host arr1 i)
      (array-ref-host arr2 i)))))

;; Specification of a rotate function
(define (rotate arr)
  (:= int i (thread-idx 0))
  (:= int x [arr i])
  (syncthreads)
  (= [arr (modulo/LS (+/LS i 1) (block-dim 0))] x))

(define switch #f)

;; Sketch of a rotate fuction
(define (rotate-sketch arr SIZE)
  (if switch
      (? (syncthreads) (void))
      (syncthreads))
  (:= int i (thread-idx 0))
  (if switch
      (? (syncthreads) (void))
      (syncthreads))
  (:= int x [arr i])
  (if- #t (if switch
              (? (syncthreads) (void))
              (syncthreads))
       (void))
  (if switch
      (? (syncthreads) (void))
      (syncthreads))
  (= [arr (modulo/LS (+/LS i (??)) SIZE)] x)
  (if switch
      (? (syncthreads) (void))
      (syncthreads)))

(define SIZE 20)
(define in0 (make-array (for/vector ([i SIZE]) (make-element i)) SIZE))
(define in1 (make-array (for/vector ([i 5]) (make-element i)) 5))
(define in2 (make-array (for/vector ([i 4]) (make-element i)) 4))
(define out0 (make-array (for/vector ([i SIZE]) (make-element (modulo (+ i -1) SIZE))) SIZE))
(define out2 (make-array (for/vector ([i 5]) (make-element (modulo (+ i -1) 4))) 4))

(define (spec-rotate)
  (begin
    (invoke-kernel rotate-sketch '(1) (list SIZE) in0 SIZE)
    (invoke-kernel rotate-sketch '(1) '(4) in2 4)
    (for ([i SIZE]) (assert (eq? (array-ref-host in0 i) (array-ref-host out0 i))))
    (for ([i 4]) (assert (eq? (array-ref-host in2 i) (array-ref-host out2 i))))))

(define (synth-rotate)
  (time (synthesize #:forall '()
                    #:guarantee (spec-rotate))))


;(define (synth-rotate)
;  (define-values (_ cpu real gc)
;    (time-apply
;     (lambda () (synthesize #:forall '()
;                            #:guarantee (spec-rotate)))
;     '()))
;  (list cpu real gc))

(define res
  (list-ref (map syntax->datum (generate-forms (synth-rotate))) 0))

(define arr (make-array (for/vector ([i SIZE]) (make-element i)) SIZE))

;; TODO:: Check quasiquote

(define (rewrite-name name)
  (string->symbol (string-append* "res-" (list (symbol->string name)))))

(define (function res)
  (list-ref (cdr res) 0))

(define (function-name res)
  (list-ref (function res) 0))

(define (body res)
  (cdr (cdr res)))

(define (spec-rotate-opt res-f)
  (begin
    (define in0-opt (make-array (for/vector ([i SIZE]) (make-element i)) SIZE))
    (define in2-opt (make-array (for/vector ([i 4]) (make-element i)) 4))
    (invoke-kernel res-f '(1) (list SIZE) in0-opt SIZE)
    (invoke-kernel res-f '(1) '(4) in2-opt 4)
    (for ([i SIZE]) (assert (eq? (array-ref-host in0-opt i) (array-ref-host out0 i))))
    (for ([i 4]) (assert (eq? (array-ref-host in2-opt i) (array-ref-host out2 i))))))





