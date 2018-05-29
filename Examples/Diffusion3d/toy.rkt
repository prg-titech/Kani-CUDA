#lang rosette

(require "../../lang.rkt")
(require rosette/lib/synthax)

(current-bitwidth #f)

(define-synthax (arith-exp i j c tid-x tid-y c2 bx by nx ny nz depth)
  #:base (choose (??) i j c tid-x tid-y c2 bx by nx ny nz)
  #:else (choose
          (??) i j c tid-x tid-y c2 bx by nx ny nz
          ((choose + - * /)
           (arith-exp i j c tid-x tid-y c2 bx by nx ny nz (- depth 1))
           (arith-exp i j c tid-x tid-y c2 bx by nx ny nz (- depth 1)))))

(define-synthax (bool-exp i j c tid-x tid-y c2 bx by nx ny nz depth)
  #:base (choose (??) i j c tid-x tid-y c2 bx by nx ny nz
                 (- (choose i j c tid-x tid-y c2 bx by nx ny nz) (??)))
  #:else (choose
          (??) i j c tid-x tid-y c2 bx by nx ny nz
          (- (choose i j c tid-x tid-y c2 bx by nx ny nz) (??))
          (choose
           ((choose && || eq?)
            (bool-exp i j c tid-x tid-y c2 bx by nx ny nz (- depth 1))
            (bool-exp i j c tid-x tid-y c2 bx by nx ny nz (- depth 1)))
           (not (bool-exp i j c tid-x tid-y c2 bx by nx ny nz (- depth 1))))))

(define (bool-exp- i j c tid-x tid-y c2 bx by nx ny nz)
  (bool-exp i j c tid-x tid-y c2 bx by nx ny nz 3))

(define (arith-exp2- i j c tid-x tid-y c2 bx by nx ny nz)
  (if ;(? (eq? i 0)
   (eq? i 0)
   ;   (eq? i (- nx 1))
   ;   (eq? j (- ny 1)))
   (arith-exp i j c tid-x tid-y c2 bx by nx ny nz 1)
   (arith-exp i j c tid-x tid-y c2 bx by nx ny nz 1)))

(define (arith-exp1- i j c tid-x tid-y c2 bx by nx ny nz)
  (arith-exp i j c tid-x tid-y c2 bx by nx ny nz 1))

(define (spec-bool-exp file)
  (define in (open-input-file file))
  (define stmt "")
  (set! stmt (read-line in))
  (while (not (eof-object? stmt))
         ;(for ([i 60])
         (let ([lst (map string->number (string-split stmt " "))])
           ;(println lst)
           (if (list-ref lst 3)
               (assert
                (not
                 (bool-exp- (list-ref lst 4)
                            (list-ref lst 5)
                            (list-ref lst 6)
                            (list-ref lst 7)
                            (list-ref lst 8)
                            (list-ref lst 9)
                            (list-ref lst 10)
                            (list-ref lst 11)
                            (list-ref lst 12)
                            (list-ref lst 13)
                            (list-ref lst 14))))
               (assert
                (bool-exp- (list-ref lst 4)
                           (list-ref lst 5)
                           (list-ref lst 6)
                           (list-ref lst 7)
                           (list-ref lst 8)
                           (list-ref lst 9)
                           (list-ref lst 10)
                           (list-ref lst 11)
                           (list-ref lst 12)
                           (list-ref lst 13)
                           (list-ref lst 14))))
           (set! stmt (read-line in))))
  (close-input-port in))

(define (spec-arith1-exp file)
  (define in (open-input-file file))
  (define stmt "")
  (set! stmt (read-line in))
  (while (not (eof-object? stmt))
         ;(for ([i 60])
         (let ([lst (map string->number (string-split stmt " "))])
           ;(println lst)
           (when (not (list-ref lst 3))
             (assert
              (eq? (list-ref lst 2)
                   (arith-exp1- (list-ref lst 4)
                                (list-ref lst 5)
                                (list-ref lst 6)
                                (list-ref lst 7)
                                (list-ref lst 8)
                                (list-ref lst 9)
                                (list-ref lst 10)
                                (list-ref lst 11)
                                (list-ref lst 12)
                                (list-ref lst 13)
                                (list-ref lst 14)))))
           (set! stmt (read-line in))))
  (close-input-port in))

(define (spec-arith2-exp file)
  (define in (open-input-file file))
  (define stmt "")
  (set! stmt (read-line in))
  (while (not (eof-object? stmt))
         ;(for ([i 60])
         (let ([lst (map string->number (string-split stmt " "))])
           ;(println lst)
           (when (list-ref lst 3)
             (assert
              (eq? (list-ref lst 3)
                   (arith-exp2- (list-ref lst 4)
                                (list-ref lst 5)
                                (list-ref lst 6)
                                (list-ref lst 7)
                                (list-ref lst 8)
                                (list-ref lst 9)
                                (list-ref lst 10)
                                (list-ref lst 11)
                                (list-ref lst 12)
                                (list-ref lst 13)
                                (list-ref lst 14)))))
           (set! stmt (read-line in))))
  (close-input-port in))

(define (synth-bool-exp file)
  (map syntax->datum
       (generate-forms
        (time
         (synthesize #:forall '()
                     #:guarantee (spec-bool-exp file))))))

(define (synth-bool-exp2 file1 file2)
  (map syntax->datum
       (generate-forms
        (time
         (synthesize #:forall '()
                     #:guarantee (begin
                                   (spec-bool-exp file1)
                                   (spec-bool-exp file2)))))))

;(synth-bool-exp "profile.rkt")

;; Desired exp:  (i = 0) ? c1 : c1 - 1
;; cpu time: 65762 real time: 678763 gc time: 5165
;; '(if (not (eq? (eq? 6 i) (eq? (- i -4) 7))))
;; cpu time: 82228 real time: 1815834 gc time: 20059
;; '(if (not (eq? (eq? 0 tid-x) (eq? i 0))))
;; cpu time: 34482 real time: 522618 gc time: 2127
;; '(if (eq? (eq? (- i 7) -1) (not (eq? bx i))))
;; cpu time: 83219 real time: 1613923 gc time: 7061
;; '(if (eq? (not (eq? nz i)) (eq? (- bx -3) i)))
;; Result: cpu time: 20791 real time: 35731 gc time: 2874
;; '((define (arith-exp- i j c tid-x tid-y c2 bx by nx ny nz) (if (not (eq? 0 i)) (- c1 1) (/ c1 1))))
;; cpu time: 49460 real time: 528290 gc time: 2149
;; '((define (arith-exp- i j c tid-x tid-y c2 bx by nx ny nz) (if (not (eq? 0 i)) (- (- c2 1) 0) (- (/ c2 1) 0))))
;; cpu time: 4953 real time: 245075 gc time: 109
;; '(if (smem (if (eq? i 0) c2 (+ -1 c2))))


;; Desired exp:  (j = 0) ? c1 : c1 - blockdim.x
;; cpu time: 29123 real time: 52096 gc time: 1216
;; '(not (eq? (eq? 0 j) (eq? 0 tid-y)))
;; cpu time: 11774 real time: 269552 gc time: 471
;; '(if (eq? j 0) c2 (- (/ c2 1) (/ bx 1)))
(define (synth-arith1-exp file)
  (map syntax->datum
       (generate-forms
        (time
         (synthesize #:forall '()
                     #:guarantee (spec-arith1-exp file))))))

(define (synth-arith2-exp file)
  (map syntax->datum
       (generate-forms
        (time
         (synthesize #:forall '()
                     #:guarantee (spec-arith2-exp file))))))

(define (main file file2)
  (println
   (list 'if
         (list-ref (list-ref (synth-bool-exp2 file file2) 0) 2))))
         ;(list-ref (list-ref (synth-bool-exp file file2) 0) 2))))
         ;(list 'smem (list-ref (list-ref (synth-arith2-exp file) 0) 2)))))
         ;(list 'in (list-ref (list-ref (synth-arith1-exp file) 0) 2)))))

(main "profile.rkt" "profile-.rkt")