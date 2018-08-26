#lang racket/base

(require syntax/parse
         (for-syntax racket/base))

(provide make-tag-function)

; use a syntax parser because I don't want to learn the
; intricacies of `match`, specifically using `...`
(define (to-txexpr l)
  (set! l (datum->syntax #f l))
  (syntax-parse l
    [(proc-name:id (kws:expr ...)
                   (kw-args:expr ...)
                   (normal-args:expr ...))
     (syntax->datum #`(proc-name ((kws kw-args) ...) normal-args ...))]))

(define (make-tag-function tag-name)
  (procedure-rename (make-keyword-procedure (λ (kws kw-args . rest)
                            (to-txexpr (list tag-name
                                             (map (λ (keyword)
                                                    (string->symbol (keyword->string keyword)))
                                                  kws)
                                             kw-args
                                             rest))))
                    (string->symbol (format "frollen-tag-function:~a" tag-name))))