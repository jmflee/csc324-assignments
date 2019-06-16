#lang racket
#| Assignment 2 - Classes

This file contains your work for Questions 1 and 2, extending the basic
class macro to include support for traits and some basic introspection.
|#
(provide class-meta class-trait)

; QUESTION 1 (metaprogramming).
(define-syntax class-meta
  (syntax-rules ()
    [(class-meta <Class> (<attr> ...)
       [(<method> <param> ...) <body>] ...)
     (define (<Class> <attr> ...)
       (λ(msg)
         (cond [(equal? msg "_attributes") ; Get attributes
                (sort (list (list (id->string <attr>) <attr>) ...)
                      #:key first string<?)] ; Sort attributes relative to their names
               [(equal? msg "_methods") ; Get methods
                (sort (list (list (id->string <method>) (λ(<param> ...) <body>)) ...)
                      #:key first string<?)] ; Sort methods relative to thier names
               [(equal? msg (id->string <attr>)) <attr>]
               ...
               [(equal? msg (id->string <method>))
                (λ(<param> ...) <body>)]
               ...
               [else "Unrecognized message!"]))
       )]))

; QUESTION 2 (traits).
(define-syntax class-trait
  (syntax-rules (with)
    [(class-trait <Class> (<attr> ...) (with <trait> ...)
                  [(<method> <param> ...) <body>] ...)
     (define (<Class> <attr> ...)
       (foldl (λ(c t) (c t)) ; Fold left to right for traits with the same method name
              (λ(msg)
                (cond [(equal? msg (id->string <attr>)) <attr>]
                      ...
                      [(equal? msg (id->string <method>))
                       (λ(<param> ...) <body>)]
                      ...
                      [else "Unrecognized message!"]))
              (list <trait> ...) ; Adds traits to fold
              ))]))

; -----------------------------------------------------------------------------
; Class macro. This section is just for your reference.
; -----------------------------------------------------------------------------
(define-syntax class
  (syntax-rules ()
    [(class <Class> (<attr> ...)
       [(<method> <param> ...) <body>] ...)
     (define (<Class> <attr> ...)
       (lambda (msg)
         (cond [(equal? msg (id->string <attr>)) <attr>]
               ...
               [(equal? msg (id->string <method>))
                (lambda (<param> ...) <body>)]
               ...
               [else "Unrecognized message!"]))
       )]))

(define-syntax id->string
  (syntax-rules ()
    [(id->string <id>)
     (symbol->string (quote <id>))]))
