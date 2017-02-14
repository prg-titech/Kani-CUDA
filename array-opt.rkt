#lang rosette

(require "work.rkt" "memory.rkt")

(provide array array-contents element-content read-reset! write-reset! read/B-reset! write/B-reset!
         make-element new-vec vec-set! array-ref! array-set! array-set-dim!
         memory-contents make-array make-shared-array
         print-matrix array-set-host! array-ref-host)

(define switch 1)

;; Structure of element of array 
(struct element
  ([content #:mutable] ;int or boolean
   [read #:mutable]    ;int or boolean
   [write #:mutable]   ;int or boolean
   [read/B #:mutable]  ;int or boolean 
   [write/B #:mutable]);int or boolean
  #:property prop:procedure
  (lambda (elem) (element-content elem)))

;; Make new element
(define (make-element e)
  (element e #f #f #f #f))

;; Return element rewrited its read to #f
(define (read-reset! elem)
  (set-element-read! elem #f)
  elem)

;; Return element rewrited its write to #f
(define (write-reset! elem)
  (set-element-write! elem #f)
  elem)

;; Return element rewrited its read/B to #f
(define (read/B-reset! elem)
  (set-element-read/B! elem #f)
  elem)

;; Return element rewrited its write/B to #f
(define (write/B-reset! elem)
  (set-element-write/B! elem #f)
  elem)

;; Structure of array
;; TODO? type check
(struct array
  ([contents #:mutable]  ;vector of element
   [dimension #:mutable]);list of int
  #:property prop:procedure
  (lambda (arr . ixs)
    (array-ref-dim! arr ixs)))

;; Print an array as a m * n matrix
(define (print-matrix arr n m)
  (let* ([cont (array-contents arr)])
    (for ([j m])
      (for ([i n])
        (printf "~a " (element-content (vector-ref cont (+ i (* j n))))))
      (newline))))

;; Make a new array
(define (make-array vec . dim)
  (define arr (array vec dim))
  (memory-allocate! arr)
  arr)

;; Check whether all the elements of vector is same
(define (check-const vec)
  (if (vector? vec)
      (let ([elem (vector-ref vec 0)])
        (for/and ([i (block-size)])
          (eq? elem (vector-ref vec i))))
      #f))

;; Make a new shared array
(define (make-shared-array-content type dim)
  (define arr (new-symbolic-array type dim))
  (shared-memory-allocate! arr)
  arr)

(define (make-shared-array type . dim)
  (cond
    [(eq? (length dim) 1) (let ([elem (list-ref dim 0)])
                            (if (check-const elem)
                                (make-shared-array-content type (list (vector-ref elem 0)))
                                (make-shared-array-content type dim)))]
    [else (let ([elem1 (list-ref dim 0)]
                [elem2 (list-ref dim 1)])
            (if (check-const elem1)
                (if (check-const elem2)
                    (make-shared-array-content type (list (vector-ref elem1 0) (vector-ref elem2 0)))
                    (make-shared-array-content type (list (vector-ref elem1 0) elem2)))
                (if (check-const elem2)
                    (make-shared-array-content type (list elem1 (vector-ref elem2 0)))
                    (make-shared-array-content type (list elem1 elem2)))))]))



;; Make a symbolic vector with length ``n'' and type ``type''
(define (new-symbolic-vector n type)
  (for/vector ([i (in-range n)])
    (define-symbolic* x type)
    x))

;; Make an array consisting elements containing a symbolic value
(define (new-symbolic-array type dim)
  (let ([n (apply * dim)])
    (array
     (for/vector ([i (in-range n)])
       (define-symbolic* x type)
       (make-element x))
     dim)))

;(define (new-symbolic-vector n type)

;; Create a new vector value with type ``type''
(define (new-vec type)
  (new-symbolic-vector (block-size) type))

;; Create a scalar value from scalar ``s''
(define (scalar->vec s)
  (make-vector (block-size) s))

;; Create a new array with length n and type ``type''
(define (new-sh-array n type)
  (new-symbolic-vector n type))

;; Denotation of the statement ``xs = vs''
;; Assign each element of vs to xs, except masked values
(define (vec-set-const! xs vs)
  (for ([i (in-range (block-size))]
        [m (mask)]
        [v (vecfy vs)])
    (when m (vector-set! xs i v))))

(define (vec-set! xs vs)
  (for*/all ([xs xs]
             [vs vs])
    (vec-set-const! xs vs)))

;; DONE; implement Read/Write set
;; Denotation of an expression arr[ixs]
;; If a thread is masked, array-ref! returns the special symbol 'masked-value
(define (array-ref-const! arr ixs)
  (for/vector ([tid (tid)] 
               [i (vecfy ixs)]
               [m (mask)])
    (if m
        (let* ([bid (bid)]
               [vec (array-contents arr)]
               [elem (vector-ref vec i)]
               [cont (element-content elem)]
               [read (element-read elem)]
               [write (element-write elem)]
               [read/B (element-read/B elem)]
               [write/B (element-write/B elem)])
          ;(print read)
          ;(newline)
          ;(print write)
          ;(newline)
          (if (or (eq? write tid) (eq? write #f))
              (if (or (eq? write/B bid) (eq? write/B #f))
                  (begin
                    (set-element-read/B! elem bid)
                    (cond
                      [(eq? read #f)
                       ;; If this element is not read, its read set is rewritten to tid
                       (begin
                         (set-element-read! elem tid)
                         cont)]
                      [(eq? read tid)
                       ;; If this element is read in this thread, its read set is through
                       cont]
                      [else
                       ;; If this element is read in a other thread, its read is rewritten to -1
                       (begin
                         (set-element-read! elem -1)
                         cont)]))
                  (assert false))
              (assert false)))
        'masked-value)))

(define (array-ref! arr ixs)
  (for*/all ([ixs ixs]
             [m (mask)]
             [arr arr])
    (parameterize ([mask m])
      (array-ref-const! arr ixs)
      )))

(define (array-ref-dim! arr ixs)
  (let* ([dim (array-dimension arr)]
         [size (length dim)])
    (cond
      [(not (eq? size (length ixs))) (assert false)]
      [(eq? size 1) (let ([id (list-ref ixs 0)]) (array-ref! arr id))]
      [(eq? size 2) (let ([ixs0 (vecfy (list-ref ixs 0))]
                          [ixs1 (vecfy (list-ref ixs 1))])
                      (define id
                        (for/vector ([i (block-size)])
                          (let ([ix0 (vector-ref ixs0 i)]
                                [ix1 (vector-ref ixs1 i)])
                            (if (and (< ix0 (list-ref dim 0)) (< ix1 (list-ref dim 1)))
                                (+ (* ix0 (list-ref dim 1)) ix1)
                                (assert false)))))
                      (array-ref! arr id))])))

(define (array-ref-host arr ix)
  (let ([cont (array-contents arr)])
    (element-content (vector-ref cont ix))))

;; Denotation of the statement arr[ixs] = vs
;; array-set! assigns vs to each elements of arr[ixs]
(define (array-set-const! arr ixs vs)
  (for ([tid (tid)]
        [m (mask)]
        [i (vecfy ixs)]
        [v (vecfy vs)])
    ;;(printf "m, i, v = ~a, ~a, ~a\n" m i v)
    (when m
      (let* ([bid (bid)]
             [vec (array-contents arr)]
             [elem (vector-ref vec i)]
             [cont (element-content elem)]
             [read (element-read elem)]
             [write (element-write elem)]
             [read/B (element-read/B elem)]
             [write/B (element-write/B elem)])
        (if (and (or (eq? write tid) (eq? write #f)) (or (eq? read tid) (eq? read #f)))
            (if (and (or (eq? write/B bid) (eq? write/B #f)) (or (eq? read/B bid) (eq? read/B #f)))
                (begin
                  (set-element-write/B! elem bid)
                  (cond
                    [(eq? write #f)
                     ;; If this element is not written, its write set is rewritten to tid
                     (begin
                       (set-element-write! elem tid)
                       (set-element-content! elem v))]
                    [(eq? write tid)
                     ;; If this element is written in this thread, its write set is through
                     (set-element-content! elem v)]
                    [else
                     ;; If this element is written in a other thread, its write set is rewritten to -1
                     (begin
                       (set-element-write! elem -1)
                       (set-element-content! elem v))]))
                (assert false))
            (assert false))))))

(define (array-set! arr ixs vs)
  (for*/all ([ixs ixs]
             [m (mask)]
             [arr arr]
             [vs vs])
    (parameterize ([mask m])
      (array-set-const! arr ixs vs)
      )))

(define (array-set-dim! arr vs . ixs)
  (let* ([dim (array-dimension arr)]
         [size (length dim)])
    (cond
      [(not (eq? size (length ixs))) (assert false)]
      [(eq? size 1) (let ([id (list-ref ixs 0)]) (array-set! arr id vs))]
      [(eq? size 2) (let ([ixs0 (vecfy (list-ref ixs 0))]
                          [ixs1 (vecfy (list-ref ixs 1))])
                      (define id
                        (for/vector ([i (block-size)])
                          (let ([ix0 (vector-ref ixs0 i)]
                                [ix1 (vector-ref ixs1 i)])
                            (if (and (< ix0 (list-ref dim 0)) (< ix1 (list-ref dim 1)))
                                (+ (* ix0 (list-ref dim 1)) ix1)
                                (assert false)))))
                      (array-set! arr id vs))])))

(define (array-set-host! arr ix v)
  (let ([cont (array-contents arr)])
    (set-element-content! (vector-ref cont ix) v)))
