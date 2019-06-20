#lang rosette

(require "../../lang.rkt")
(require rosette/lib/synthax)

(current-bitwidth #f)

(define-synthax (arith-exp bdimx c2 row col brow bcol trow tcol i k depth)
  #:base (choose 0 1 2 bdimx c2 row col brow bcol trow tcol i k)
  #:else (choose
          0 1 2 bdimx c2 row col brow bcol trow tcol i k
          ((choose + *)
           (arith-exp bdimx c2 row col brow bcol trow tcol i k (- depth 1))
           (arith-exp bdimx c2 row col brow bcol trow tcol i k (- depth 1)))))

(define (arith-exp- bdimx c2 row col brow bcol trow tcol i k)
  (arith-exp bdimx c2 row col brow bcol trow tcol i k 2))

(define (spec-arith-exp file)
  (define in (open-input-file file))
  (define stmt "")
  (set! stmt (read-line in))
  (while (not (eof-object? stmt))
         (let ([lst (map string->number (string-split stmt " "))])
           (when (list-ref lst 3)
             (assert
              (eq? (list-ref lst 3)
                   (arith-exp- (list-ref lst 4)
                               (list-ref lst 5)
                               (list-ref lst 6)
                               (list-ref lst 7)
                               (list-ref lst 8)
                               (list-ref lst 9)
                               (list-ref lst 10)
                               (list-ref lst 11)
                               (list-ref lst 12)
                               (list-ref lst 13)))))
           (set! stmt (read-line in))))
  (close-input-port in))
  ;(println (asserts)))

(define (synth-arith-exp file)
  (map syntax->datum
       (generate-forms
        (time
         (synthesize #:forall '()
                     #:guarantee 
                     (spec-arith-exp file)
                     )))))

(synth-arith-exp "profile.rkt")


;; desired expression: k + (bdimx * tidx)

;; cpu time: 1828 real time: 10305 gc time: 361
;; block 3*3 , grid 3*3
;; result: k + (2 * tidx) + (bdimx * (tidy + bidx))

;; cpu time: 1848 real time: 18192 gc time: 130
;; block 4*4, grid 3*3
;; result: k + tidx + bdimx * tidy