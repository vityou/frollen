#lang racket/base

(require (for-syntax syntax/parse
                     racket/base)
         "tag.rkt"
         racket/pretty
         syntax/wrap-modbeg
         racket/list)

(provide (for-syntax make-accumulating-module-begin)
         tag-top)





; appends all expressions to the global variable `acc`
; and handles meta deffinitions
(define-syntax handle
  (syntax-parser
    [(_ (_ (~datum define-meta) first ...))
     #:with metas (syntax-local-introduce (datum->syntax #f 'metas))
     (syntax-parse #'(define-meta first ...)
       [(_ meta-id:id value:expr) #'(hash-set! metas 'meta-id value)])]
    [(_ b:expr)
     ; make sure we are talking about the `acc` in the created module
     ; not an `acc` defined here
     #:with acc (syntax-local-introduce (datum->syntax #f 'acc))
     #'(set! acc
             (append acc (list b)))]))

; this makes a module-begin that wraps all top level expressions
; (except declararations like `define` and `require`) in `handle`
(define-syntax wrapping-modbeg
  (make-wrapping-module-begin #'handle #'#%module-begin))


; allows users to specify their own function to apply to
; the acc list before it is exported as doc
(define-for-syntax (make-accumulating-module-begin parser)
  (syntax-parser
    [(_ expr1 ...)
     ; make sure we are talking about the `acc` in the created module
     ; not an `acc` defined here (same as in `handle`)
     #:with acc (syntax-local-introduce (datum->syntax #f 'acc))
     #:with metas (syntax-local-introduce (datum->syntax #f 'metas))
     #:with root (datum->syntax this-syntax 'root)
     #`(wrapping-modbeg (define acc '())
                        (define metas (make-hash))
                        expr1 ...
                        ; remove leading newlines and apply
                        ; the parser function that user
                        ; gives us
                        (define doc (#,parser (drop-leading-newlines acc)
                                              root))
                        (provide doc metas)
                        (define #,(datum->syntax this-syntax 'doc) doc)
                        (define #,(datum->syntax this-syntax 'metas) metas)
                        ;(displayln "metas:\n")
                        ;(pretty-print metas)
                        ;(displayln "\naccumulated values:\n")
                        ;(pretty-print acc)
                        ;(displayln "\nafter parsing:\n")
                        #;(pretty-print doc))]))


(define-syntax tag-top
    (syntax-parser [(_ . id) #'(make-tag-function 'id)]))

(define (drop-leading-newlines l)
  (if (not (equal? (first l) "\n"))
      l
      (drop-leading-newlines (rest l))))