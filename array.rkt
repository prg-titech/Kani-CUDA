#lang rosette

(require "work.rkt" "memory.rkt")

(provide array array-contents element-content read-reset! write-reset!
         make-element new-vec vec-set! array-ref! array-set!
         memory-contents memory-length make-array)

;; element of array 
(struct element
  ([content #:mutable] ;int or boolean
   [read #:mutable]    ;int or boolean
   [write #:mutable]) ;int or boolean
  #:property prop:procedure
  (lambda (elem) (element-content elem)))

;; make new element
(define (make-element e)
  (element e #f #f))

;; return element rewrited its read to #f
(define (read-reset! elem)
  (set-element-read! elem #f)
  elem)

;; return element rewrited its write to #f
(define (write-reset! elem)
  (set-element-write! elem #f)
  elem)

;; array
;; type check?
(struct array
  ([contents #:mutable])
  #:property prop:procedure
  (lambda (arr idx)
    (array-ref! arr idx)))

;; make new array
(define (make-array vec)
  (define arr (array vec))
  (memory-set! global-memory arr)
  arr)

;; make a symbolic vector with length ``n'' and type ``type''
(define (new-symbolic-vector n type)
  (for/vector ([i (in-range n)])
    (define-symbolic* x type)
    x))

;(define (new-symbolic-vector n type)

;; create a new vector value with type ``type''
(define (new-vec type)
  (new-symbolic-vector (ntid) type))

;; create a scalar value from scalar ``s''
(define (scalar->vec s)
  (make-vector (ntid) s))

;; create a new array with length n and type ``type''
(define (new-sh-array n type)
  (new-symbolic-vector n type))

;; denotation of the statement ``xs = vs''
;; assign each element of vs to xs, except masked values
(define (vec-set-const! xs vs)
  (for ([i (in-range (ntid))]
        [m (mask)]
        [v (vecfy vs)])
    (when m (vector-set! xs i v))))

(define (vec-set! xs vs)
  (for*/all ([xs xs]
             [vs vs])
    (vec-set-const! xs vs)))

;; TODO; implement Read/Write set
;; denotation of an expression arr[ixs]
;; if a thread is masked, array-ref! returns the special symbol 'masked-value
(define (array-ref-const! arr ixs)
  (for/vector ([tid (tid)] 
               [i (vecfy ixs)]
               [m (mask)])
    (if m (let* ([vec (array-contents arr)]
                 [elem (vector-ref vec i)]
                 [cont (element-content elem)]
                 [read (element-read elem)]
                 [write (element-write elem)])
            ;(print write)
            (if (or (eq? write tid) (eq? write #f))
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
                     cont)])
                (assert false)))
        'masked-value)))

(define (array-ref! arr ixs)
  (for*/all ([ixs ixs]
             [m (mask)]
             [arr arr])
    (parameterize ([mask m])
      (array-ref-const! arr ixs)
      )))

;; denotation of the statement arr[ixs] = vs
;; array-set! assigns vs to each elements of arr[ixs]
(define (array-set-const! arr ixs vs)
  (for ([tid (tid)]
        [m (mask)]
        [i (vecfy ixs)]
        [v (vecfy vs)])
    ;;(printf "m, i, v = ~a, ~a, ~a\n" m i v)
    (when m
      (let* ([vec (array-contents arr)]
             [elem (vector-ref vec i)]
             [cont (element-content elem)]
             [read (element-read elem)]
             [write (element-write elem)])
        (if (and (or (eq? write tid) (eq? write #f)) (or (eq? read tid) (eq? read #f)))
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
                 (set-element-content! elem v))])
            (assert false))))))

(define (array-set! arr ixs vs)
  (for*/all ([ixs ixs]
             [m (mask)]
             [arr arr]
             [vs vs])
    (parameterize ([mask m])
      (array-set-const! arr ixs vs)
      )))
