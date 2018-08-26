#lang racket/base

(require (for-syntax syntax/parse
                     racket/base)
         "tag.rkt"
         racket/pretty
         syntax/wrap-modbeg)

(provide (for-syntax make-accumulating-module-begin)
         to-string
         tag-top)


(define (to-string x)
  (cond
    [(string? x) x]
    [(or (null? x) (void? x)) ""]
    [(or (symbol? x) (number? x) (path? x) (char? x)) (format "~a" x)]
    ;; special handling for procedures, because if a procedure reaches this func,
    ;; it usually indicates a failed attempt to use a tag function.
    ;; meaning, it's more useful to raise an error.
    [(procedure? x) (error 'pollen "Can't convert procedure ~a to string" x)]
    [else (format "~v" x)]))


; converts all expressions into strings and appends
; them to the global variable `acc`
(define-syntax handle
  (syntax-parser
    ;[(_ (()))]
    [(_ b:expr)
     ; make sure we are talking about the `acc` in the created module
     ; not an `acc` defined here
     #:with acc (syntax-local-introduce (datum->syntax #f 'acc))
     #'(set! acc
             (string-append acc (to-string b)))]))

; this makes a module-begin that wraps all top level expressions
; (except declararations like `define` and `require`) in `handle`
(define-syntax wrapping-modbeg
  (make-wrapping-module-begin #'handle))


; allows users to specify their own function to apply to
; the acc string before it is exported as doc
(define-for-syntax (make-accumulating-module-begin string-parser)
  (syntax-parser
    [(_ (expr1 ...))
     ; make sure we are talking about the `acc` in the created module
     ; not an `acc` defined here (same as in `handle`)
     #:with acc (syntax-local-introduce (datum->syntax #f 'acc))
     #:with metas (syntax-local-introduce (datum->syntax #f 'metas))
     #:with root (datum->syntax this-syntax 'root)
     #`(wrapping-modbeg (define acc "")
                        (define metas #hash())
                        expr1 ...
                        ; apply the string parser function that user
                        ; gives us
                        (define doc (apply root (#,string-parser acc)))
                        (provide doc)
                        (displayln "accumulated string:\n")
                        (pretty-print acc)
                        (displayln "\nparsed string:\n")
                        (pretty-print doc))]))


(define-syntax tag-top
    (syntax-parser [(_ . id) #'(make-tag-function 'id)]))