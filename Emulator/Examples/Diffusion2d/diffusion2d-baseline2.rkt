#lang rosette

(require "../../lang.rkt") ;"diffusion2d.rkt")

(provide diffusion2d-baseline2)

;; Sequential diffusion program.
(define (diffusion2d-baseline-iteration in
                                        out
                                        nx ny
                                        )
  
  (for ([y (in-range ny)])
    (for ([x (in-range nx)])
      (define-values (c w e n s) (values 0 0 0 0 0))
      (set! c (+ x (* y nx)))
      (set! w (if (eq? x 0) c (- c 1)))
      (set! e (if (eq? x (- nx 1)) c (+ c 1)))
      (set! n (if (eq? y 0) c (- c nx)))
      (set! s (if (eq? y (- ny 1)) c (+ c nx)))
      (array-set-host! out c
                       (+ (array-ref-host in c)
                          (array-ref-host in w)
                          (array-ref-host in e)
                          (array-ref-host in s)
                          (array-ref-host in n))))))

(define (diffusion2d-baseline2 count
                              in
                              out
                              nx ny
                              )
  (for ([i (in-range count)])
    (diffusion2d-baseline-iteration in
                                    out
                                    nx ny
                                    )
    (define temp in)
    (set! in out)
    (set! out temp)))