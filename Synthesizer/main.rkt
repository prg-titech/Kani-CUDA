#lang rosette

(require "../Translator/translator.rkt")

(define arg (current-command-line-arguments))
(define racket "/Users/akira/../../Applications/Racket v7.1/bin//racket")



(if (eq? (vector-length arg) 1)
    (begin
      (println "Translating...")
      (translate (vector-ref arg 0))
      (println "Profiling...")
      (system* racket "out.rkt")
      (println "Synthesizing..."))
    (raise "the expected number of arguments does not match the given number"))



