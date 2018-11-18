#lang rosette

(require rackunit
         "../lang.rkt")

(define BLOCKSIZE 32)

(define (main1)
  (for- [(define i 0) : (< i 10) : (++ i)]
        (print "hello")))

(main1)