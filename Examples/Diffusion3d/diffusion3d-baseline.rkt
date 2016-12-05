#lang rosette

(require "../../lang.rkt") ;"diffusion3d.rkt")

(provide diffusion3d-baseline)

;; Sequential diffusion program.
(define (diffusion3d-baseline-iteration in
                                        out
                                        nx ny nz
                                        ce cw cn cs ct cb cc)
  (for ([z (in-range nz)])
    (for ([y (in-range ny)])
      (for ([x (in-range nx)])
        (define-values (c w e n s b t) (values 0 0 0 0 0 0 0))
        (set! c (+ x (* y nx) (* z nx ny)))
        (set! w (if (eq? x 0) c (- c 1)))
        (set! e (if (eq? x (- nx 1)) c (+ c 1)))
        (set! n (if (eq? y 0) c (- c nx)))
        (set! s (if (eq? y (- ny 1)) c (+ c nx)))
        (set! b (if (eq? z 0) c (- c (* nx ny))))
        (set! t (if (eq? z (- nz 1)) c (+ c (* nx ny))))
        (array-set-host! out c
                         (+ (* cc (array-ref-host in c))
                            (* cw (array-ref-host in w))
                            (* ce (array-ref-host in e))
                            (* cs (array-ref-host in s))
                            (* cn (array-ref-host in n))
                            (* cb (array-ref-host in b))
                            (* ct (array-ref-host in t))))))))

(define (diffusion3d-baseline count
                              in
                              out
                              nx ny nz
                              ce cw cn cs ct cb cc)
  (for ([i (in-range count)])
    (diffusion3d-baseline-iteration in
                                    out
                                    nx ny nz
                                    ce cw cn cs ct cb cc)
    (set! in out))
  out)
