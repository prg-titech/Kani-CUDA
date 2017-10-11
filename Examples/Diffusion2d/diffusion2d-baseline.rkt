#lang rosette

(require "../../lang.rkt") ;"diffusion2d.rkt")

(provide diffusion2d-baseline)

;; Sequential diffusion program.
(define (diffusion2d-baseline-iteration in
                                        out
                                        nx ny
                                        ce cw cn cs cnw cne csw cse cc)
  
  (for ([y (in-range ny)])
    (for ([x (in-range nx)])
      (define-values (c w e n s nw ne sw se) (values 0 0 0 0 0 0 0 0 0))
      (set! c (+ x (* y nx)))
      (set! w (if (eq? x 0) c (- c 1)))
      (set! e (if (eq? x (- nx 1)) c (+ c 1)))
      (set! n (if (eq? y 0) c (- c nx)))
      (set! s (if (eq? y (- ny 1)) c (+ c nx)))
      (set! nw (if (or (eq? x 0) (eq? y 0)) c (- c nx 1)))
      (set! ne (if (or (eq? x (- nx 1)) (eq? y 0)) c (- c nx -1)))
      (set! sw (if (or (eq? x 0) (eq? y (- ny 1))) c (+ c nx -1)))
      (set! se (if (or (eq? x (- nx 1)) (eq? y (- ny 1))) c (+ c nx 1)))
      (array-set-host! out c
                       (+ (* cc (array-ref-host in c))
                          (* cw (array-ref-host in w))
                          (* ce (array-ref-host in e))
                          (* cs (array-ref-host in s))
                          (* cn (array-ref-host in n))
                          (* cnw (array-ref-host in nw))
                          (* cne (array-ref-host in ne))
                          (* csw (array-ref-host in sw))
                          (* cse (array-ref-host in se)))))))

(define (diffusion2d-baseline count
                              in
                              out
                              nx ny
                              ce cw cn cs cnw cne csw cse cc)
  (for ([i (in-range count)])
    (diffusion2d-baseline-iteration in
                                    out
                                    nx ny
                                    ce cw cn cs cnw cne csw cse cc)
    (define temp in)
    (set! in out)
    (set! out temp)))