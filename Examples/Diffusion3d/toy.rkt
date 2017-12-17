#lang rosette

(require "../../lang.rkt")

(provide (all-defined-out))

(current-bitwidth #f)

;; Specification of a rotate function
(define (rotate arr)
  (:= int i (thread-idx 0))
  (:= int x [arr i])
  (syncthreads)
  (= [arr (modulo/LS (+/LS i 1) (block-dim 0))] x))

;; Sketch of a rotate fuction
(define (rotate-sketch arr SIZE)
  (if (switch)
      (? (syncthreads) (void))
      (syncthreads))
  (:= int i (thread-idx 0))
  (if (switch)
      (? (syncthreads) (void))
      (syncthreads))
  (:= int x [arr i])
  (if- #t (if (switch)
              (? (syncthreads) (void))
              (syncthreads))
       (void))
  (if (switch)
      (? (syncthreads) (void))
      (syncthreads))
  (= [arr (modulo/LS (+/LS i (??)) SIZE)] x)
  (if (switch)
      (? (syncthreads) (void))
      (syncthreads)))
