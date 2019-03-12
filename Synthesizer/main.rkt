#lang rosette

(require "../Translator/translator.rkt")

(define arg (current-command-line-arguments))

(if (eq? (vector-length arg) 1)
    (begin
      (displayln "Translating...")
      (translate (vector-ref arg 0))
      (displayln "Profiling..."))
    (raise "the expected number of arguments does not match the given number"))



