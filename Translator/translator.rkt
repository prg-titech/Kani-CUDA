#lang rosette

(provide translate)

(require "host-translator.rkt"
         "kernel-translator.rkt"
         control
         c)

;; TODO for文に{}がない場合の変換規則
(define (desired-line? line)
  (not
   (or (string-contains? line "print")
       (string-contains? line "#"))))

(define (get-var-name line)
  (list-ref (string-split (string-trim line) #rx"[ |[]") 2))

(define (add-var-prefix line word)
  (let ([name (get-var-name line)])
    (string-replace
     (string-replace line word "")
     name
     (string-append word name))))

(define (convert-line line)
  (cond
    [(string-contains? line "__global__")
     (add-var-prefix line "__global__")]
    ;; TODO : diffusion2d_temporal_blocking.cuでおかしなことが起こってる
    [(string-contains? line "__shared__")
     (add-var-prefix line "__shared__")]
    [(string-contains? line "dim3 ")
     (string-replace line "dim3 " "")]
    [(string-contains? line "<<<")
     (let ([name (list-ref (string-split (string-trim line) #rx"[<<<]") 0)])
       (string-replace
        (string-replace
         (string-replace line "<<<" "(")
         ">>>(" ",")
        name (string-append "__global__" name)))]
    [(desired-line? line) line]
    [else ""]))

(define (cpp file)
  (define in (open-input-file file))
  (define cp (open-output-file "__cp.cu" #:exists 'truncate))
  (define line (read-line in 'any))
  (when (not (eof-object? line))
    (set! line (string-append line "\n")))
  (while (not (eof-object? line)) 
         (when (not (string-contains? line "#include"))
           (fprintf cp line))
         (set! line (read-line in 'any))
         (when (not (eof-object? line))
           (set! line (string-append line "\n"))))
  (close-input-port in)
  (close-output-port cp)
  (system* "/usr/bin/cpp" "__cp.cu" "__dst.cu"))
  

(define (translate file)
  (cpp file)
  (define in (open-input-file "__dst.cu"))
  (define line (read-line in))
  (when (not (eof-object? line))
    (set! line (string-append line "\n")))
  (define program '())
  (while (not (eof-object? line))
         (set! program (list program (convert-line line)))
         (set! line (read-line in))
         (when (not (eof-object? line))
           (set! line (string-append line "\n"))))
  (close-input-port in)
  (define out (open-output-file "out.rkt" #:exists 'truncate))
  ;(pretty-print (string-join (flatten program)))
  (pretty-display "#lang rosette\n" out)
  (pretty-display "(require \"../Emulator/lang.rkt\")" out)
  (pretty-display "(delete-directory/files \"profiles\" #:must-exist? #f)" out)
  (pretty-display "(make-directory* \"profiles\")" out)
  (for ([src (parse-program (string-join (flatten program)))])
    ;(println (symbol->string (get-name src)))
    (if (string-contains? (symbol->string (get-name src)) "__global__")
        (pretty-display (kernel-translator src) out)
        (pretty-display (host-translator src) out)))
  (pretty-display "(main)" out)
  (close-output-port out))

;(translate "/Users/akira/masuhara-lab/Kani-CUDA/Emulator/Examples/Diffusion2d/diffusion2d_temporal_blocking.cu")
