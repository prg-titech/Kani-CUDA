#lang racket

(define a (vector-ref (current-command-line-arguments) 0))
(printf "input: ~a, length: ~a, last character: ~a\n"
        a
        (string-length a)
        (char->integer (string-ref a (- (string-length a) 1))))