#lang rosette

(require "../../lang.rkt")

(provide mulmat-baseline)

(define (mulmat-baseline A B C SIZE)
  (define c 0)
  (for ([j SIZE])
    (for ([i SIZE])
      (set! c (+ i (* j SIZE)))
      (array-set-host! C c 0)
      (for ([k SIZE])
        (array-set-host! C c
                         (+ (array-ref-host C c)
                            (* (array-ref-host A (+ i (* k SIZE)))
                               (array-ref-host B (+ k (* j SIZE))))))))))