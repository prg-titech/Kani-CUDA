#lang rosette

(require rosette/lib/synthax "../../lang.rkt")

(current-bitwidth #f)

(define-synthax (arith-exp bidx j bdimx i g-loc BLOCKSIZE gdimy bidy tidy value b-loc tidx sum bdimy gdimx KERNEL-RADIUS depth)
  #:base (choose 0 1 2 bidx j bdimx i g-loc BLOCKSIZE gdimy bidy tidy value b-loc tidx sum bdimy gdimx KERNEL-RADIUS)
  #:else (choose 0 1 2 bidx j bdimx i g-loc BLOCKSIZE gdimy bidy tidy value b-loc tidx sum bdimy gdimx KERNEL-RADIUS ((choose + - *) (arith-exp bidx j bdimx i g-loc BLOCKSIZE gdimy bidy tidy value b-loc tidx sum bdimy gdimx KERNEL-RADIUS (- depth 1)) (arith-exp bidx j bdimx i g-loc BLOCKSIZE gdimy bidy tidy value b-loc tidx sum bdimy gdimx KERNEL-RADIUS (- depth 1)))))
(define (arith-exp- bidx j bdimx i g-loc BLOCKSIZE gdimy bidy tidy value b-loc tidx sum bdimy gdimx KERNEL-RADIUS) (arith-exp bidx j bdimx i g-loc BLOCKSIZE gdimy bidy tidy value b-loc tidx sum bdimy gdimx KERNEL-RADIUS 2))
(define (spec-arith-exp file)
  (define in (open-input-file file))
  (define stmt 0)
  (set! stmt (read-line in))
  (while (not (eof-object? stmt))
         (let ([lst (map string->number (string-split stmt " "))])
           (when (list-ref lst 3)
             (assert
              (eq? (list-ref lst 3)
                   (arith-exp- (list-ref lst 4) (list-ref lst 5) (list-ref lst 6) (list-ref lst 7) (list-ref lst 8) (list-ref lst 9) (list-ref lst 10) (list-ref lst 11) (list-ref lst 12) (list-ref lst 13) (list-ref lst 14) (list-ref lst 15) (list-ref lst 16) (list-ref lst 17) (list-ref lst 18) (list-ref lst 19)))))
           (set! stmt (read-line in))))
  (close-input-port in))
(list-ref (list-ref (map syntax->datum
       (generate-forms
        (time
         (synthesize #:forall '()
                     #:guarantee 
                     (spec-arith-exp "profile.rkt")
                     )))) 0) 2)