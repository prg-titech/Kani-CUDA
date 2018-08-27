#lang rosette

(require "../../lang.rkt")

(provide vec-add-baseline)

(define (vec-add-baseline A B C SIZE)
  (define c 0)
  (for ([j SIZE])
    (for ([i SIZE])
      (set! c (+ i (* j SIZE)))
      (array-set-host! C c 0)
      (array-set-host! C c
                       (+ (array-ref-host A c)
                          (array-ref-host B c))))))