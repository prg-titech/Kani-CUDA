#lang rosette

(require rackunit
         "../lang.rkt"
         "../work.rkt" 
         rosette/lib/synthax rosette/lib/angelic)

(define-symbolic b boolean?)

(check-equal?
 (?: (eq? 1 1) 1 2)
 (make-vector 16 1)
 "Simple ?:-statement")

(check-equal?
 (?: #t 1 2)
 (make-vector 16 1)
 "Simple ?:-statement")
