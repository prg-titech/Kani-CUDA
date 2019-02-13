#lang racket


(define (head lst)
  (list-ref lst 0))

;; convert-op :: symbol? -> string
(define (convert-op sym)
  (cond [(eq? sym 'neq?/LS)     "!="]
        [(eq? sym '!/LS)        "!"]
        [(eq? sym 'eq?/LS)      "=="]
        [(eq? sym 'eq?)         "=="]
        [(eq? sym '||/LS)       "||"]
        [(eq? sym '&&/LS)       "&&"]
        [(eq? sym '&/LS)        "&"]
        [(eq? sym '&)           "&"]
        [(eq? sym '++/LS)       "++"]
        [(eq? sym '+)           "++"]
        [(eq? sym '--/LS)       "--"]
        [(eq? sym '-)           "++"]
        [(eq? sym '</LS)        "<"]
        [(eq? sym '<)           "<"]
        [(eq? sym '>/LS)        ">"]
        [(eq? sym '>)           ">"]
        [(eq? sym 'eq?)         "=="]
        [(eq? sym '+/LS)        "+"]
        [(eq? sym '+)           "+"]
        [(eq? sym '-/LS)        "-"]
        [(eq? sym '-)           "-"]
        [(eq? sym '*/LS)        "*"]
        [(eq? sym '*)           "*"]
        [(eq? sym 'quotient/LS) "/"]
        [(eq? sym 'quotient)    "/"]
        [else "xxx"]))
         

;; translate-to-cuda :: list? -> string?
(define (translate-to-cuda lst)
  (match lst
    [(list 'define (list name arg ...) body ...)
     (string-append
      (string-append* "void "
                      (translate-to-cuda name)
                      "("
                      (string-trim
                       (apply
                        string-append
                        (for/list ([arg arg])
                          (string-append
                           (translate-to-cuda arg)
                           ",")))
                       ",")
                      "){\n"
                      (for/list ([line body])
                        (string-append
                         (translate-to-cuda line)
                         ";\n")))
      "}")]
    [(list unop arg)
     (string-append "("
                    (convert-op unop)
                    (translate-to-cuda arg)
                    ")")]
    [(list 'if- cond then)
     (string-append "if("
                    (translate-to-cuda cond)
                    "){"
                    (translate-to-cuda then)
                    "}")]
    [(list '= left right)
     (string-append (translate-to-cuda left)
                    "="
                    (translate-to-cuda right))]
    [(list ': type var)
     (string-append (translate-to-cuda type)
                    " "
                    (translate-to-cuda var))]
    [(list ': type (list arr size))
     (string-append (translate-to-cuda type)
                    " "
                    (translate-to-cuda arr)
                    "["
                    (translate-to-cuda size)
                    "]")]
    [(list binop left right)
     (string-append "("
                    (translate-to-cuda left)
                    (convert-op binop)
                    (translate-to-cuda right)
                    ")")]
    [(list ':= type var val)
     (string-append (translate-to-cuda type)
                    " "
                    (translate-to-cuda var)
                    "="
                    (translate-to-cuda val))]
    [(list '?: cond then else)
     (string-append "("
                    (translate-to-cuda cond)
                    ") ? "
                    (translate-to-cuda then)
                    " : "
                    (translate-to-cuda else))]
    [(list 'if- cond then else)
     (string-append "if("
                    (translate-to-cuda cond)
                    "){\n"
                    (translate-to-cuda then)
                    "}else{\n"
                    (translate-to-cuda else)
                    "}")]
    [(list 'for- (list init : cond : change) body ...)
     (string-append
      (string-append* "for("
                      (translate-to-cuda init)
                      ";"
                      (translate-to-cuda cond)
                      ";"
                      (translate-to-cuda change)
                      "){\n"
                      (for/list ([line body])
                        (string-append
                         (translate-to-cuda line)
                         ";\n")))
      "}")]
    [val
     #:when (symbol? val)
     (symbol->string val)]
    [val
     #:when (number? val)
     (number->string val)]
    ))


(translate-to-cuda '(+/LS a (*/LS b c)))
(pretty-display
 (translate-to-cuda '(define (test j k)
                       (for- [(:= int i 0) : (</LS i 10) : (++/LS i)]
                             (= j (+/LS i 1))
                             (if- (eq?/LS k 0)
                                  (= k (-/LS i 1))
                                  (= k (+/LS i 1)))))))
