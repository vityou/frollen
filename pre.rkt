#lang racket/base

(require "module-lang-utils.rkt"
         "to-string.rkt"
         racket/contract
         (for-syntax racket/base))

(provide (rename-out [md-module-begin #%module-begin]
                     [tag-top #%top])
         (except-out (all-from-out racket/base)
                     #%module-begin
                     #%top))


; create reader module
(module reader racket/base
  
  (require syntax/strip-context
           scribble/reader)

  (provide (rename-out [md-read read]
                       [md-read-syntax read-syntax]))

  ; create an @ reader to parse @ expressions
  (define my-at-reader (make-at-reader #:inside? #t
                                       #:syntax? #t))

  ; read just uses read-syntax defined below
  (define (md-read in)
    (syntax->datum
     (md-read-syntax #f in)))

  ; use the @ reader to seperate out actual expressions from text
  ; for example, the program:
  ;
  ; @string-append["hi " "there"]asdf123(+ 1 1)
  ;
  ; would become the syntax object:
  ;
  ; #'((string-append "hi "
  ;                   "there")
  ;    "asdf123(+ 1 1)")
  ;
  ; then we give the parsed expression to whatever `module-name` is
  ; which takes care of whether it should be treated as markdown
  ; or plain text or whatever
  (define (md-read-syntax src in)
    (with-syntax ([stx (my-at-reader src in)])
      (strip-context
       #`(module anything frollen/pre
           stx)))))
; end of reader module


; convert values to strings, conglomerate into 1 string,
; and ignore given root-proc
(define (accumulate-to-string list-of-values root-proc)
  (apply string-append (map to-string list-of-values)))

; create a string accumulating module begin using
; `accumulate-to-string` to parse the accumulated string
(define-syntax md-module-begin
  (make-accumulating-module-begin #'accumulate-to-string))