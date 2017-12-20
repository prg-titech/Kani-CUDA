#lang rosette

(require "../../lang.rkt" "test2.rkt")

(current-bitwidth #f)

(define (aux-insert-barrier lst)
  (for ([e lst])
    (cond
      [(eq? (car e) 'begin)
       (insert-barrier (cdr e))]
      [(member (car e) (list 'while 'for))
       (insert-barrier (cddr e))]
      [(member (car e) (list 'if 'if-))
       (begin
         (insert-barrier (list (list-ref e 2)))
         (insert-barrier (list (list-ref e 3))))]
      [else
       23]))
  (add-between lst
               '(if (switch)
                    (? (syncthreads) (void))
                    (syncthreads))))

(define (insert-barrier def)
  (append
   (list 'define (cadr def))
   (aux-insert-barrier (cddr def))))

(define (main path)
  (define in (open-input-file path))
  (read-line in)
  (define stmt #f)
  (define lst null)
  (set! stmt (read in))
  (while (not (eof-object? stmt))
         (if (eq? (car stmt) 'define)
             (set! lst (append lst (list (insert-barrier stmt))))
             (set! lst (append lst (list stmt)))) 
         (set! stmt (read in)))
  (close-input-port in)
  (define out (open-output-file "sketch.rkt" #:exists 'truncate))
  (fprintf out "#lang rosette\n")
  (for-each (lambda (e) (writeln e out))
            lst)

  (for-each (lambda (e) (writeln e out))
            '(;; Specification of a rotate function
              (define (rotate arr)
                (:= int i (thread-idx 0))
                (:= int x [arr i])
                (syncthreads)
                (= [arr (modulo/LS (+/LS i 1) (block-dim 0))] x))
              
              (define (array-eq-assert arr1 arr2 len)
                (for ([i (in-range len)])
                  (assert
                   (eq?
                    (array-ref-host arr1 i)
                    (array-ref-host arr2 i)))))
              
              (define (r)
                (define-symbolic* r real?)
                r)
              (define SIZE 20)
              ;(: int in0[SIZE])
              ;(: int in1[5])
              (define in0 (make-array (for/vector ([i SIZE]) (make-element i)) SIZE))
              (define in1 (make-array (for/vector ([i 5]) (make-element i)) 5))
              (define out0 (make-array (for/vector ([i SIZE]) (make-element (modulo (+ i -1) SIZE))) SIZE))
              (define out1 (make-array (for/vector ([i 5]) (make-element (modulo (+ i -1) 5))) 5))
              
              (define (spec-rotate)
                (begin
                  (invoke-kernel rotate-sketch '(1) (list SIZE) in0 SIZE)
                  (invoke-kernel rotate-sketch '(1) '(5) in1 5)
                  (array-eq-assert in0 out0 SIZE)
                  (array-eq-assert in1 out1 5)))
              
              (define (synth-rotate)
                (time (synthesize #:forall '()
                                  #:guarantee (spec-rotate))))
              
              (define res
                (list-ref (map syntax->datum  (generate-forms (synth-rotate))) 0))
              
              (define test
                '((define (array-eq-assert arr1 arr2 len)
                    (for ([i (in-range len)])
                      (assert
                       (eq?
                        (array-ref-host arr1 i)
                        (array-ref-host arr2 i)))))
                  (define (spec-rotate-opt res-f)
                    (define SIZE 20)
                    (define in0-opt (make-array (for/vector ([i SIZE]) (make-element i)) SIZE))
                    (define in1-opt (make-array (for/vector ([i 5]) (make-element i)) 5))
                    (define out0 (make-array (for/vector ([i SIZE]) (make-element (modulo (+ i -1) SIZE))) SIZE))
                    (define out1 (make-array (for/vector ([i 5]) (make-element (modulo (+ i -1) 5))) 5))
                    (invoke-kernel res-f '(1) (list SIZE) in0-opt SIZE)
                    (invoke-kernel res-f '(1) '(5) in1-opt 5)
                    (array-eq-assert in0-opt out0 SIZE)
                    (array-eq-assert in1-opt out1 5))))
              
              (write-synth-result res test)))
  
  (close-output-port out)
  
  (define path-to-racket-exe
    "/Applications/Racket v6.6/bin//racket")
  (define path-to-rosette-code1
    "/Users/akira/Masuhara Lab/Kani-CUDA/Examples/Diffusion3d/sketch.rkt")
  (define path-to-rosette-code2
    "/Users/akira/Masuhara Lab/Kani-CUDA/Examples/Diffusion3d/res.rkt")
  
  (system*
   path-to-racket-exe
   path-to-rosette-code1)
  
  (system*
   path-to-racket-exe
   path-to-rosette-code2))

(main "toy.rkt")

