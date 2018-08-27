#lang rosette

(require "../../lang.rkt")

(provide (all-defined-out))

(current-bitwidth #f)

;; Sketch of a rotate fuction
(define (rotate-sketch arr SIZE)
  (:= int i (thread-idx 0))
  (:shared real smem[(block-size)])
  (= [smem i] [arr i])
  (:= int x (profiling-access arr i))
  (= [arr (modulo/LS (+/LS i (??)) SIZE)] x))
