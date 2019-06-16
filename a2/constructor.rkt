#lang racket
#|
For my macro of the python constructor, I tried tying the format as closely to the python
syntax with the only difference being that there is a lot more brackets compared to python
but it still beats the python interpreter screaming at you for a misplace space or tab.
The format follows as the standard class definition in class.rkt except now, there is an
additional optional parameters the user can assign when creating constructor variables,
one line of input in the form (self.variable body) ...

Ex.

(class-constructor MyClass            ; class MyClass:
                   (a b)            ; def __init__(self, a, b):
                   [(r (f a))           ; r = f(a)
                    (x (f a))           ; self.x = f(a)
                    (y (list b 100 r))  ; self.y = [b, 100, r]
                    (z "you are cool")] ; self.z = "you are cool"
                   [(square n)      ; def square(n):
                    (* n n)])      ;      return n*n

(define (f r) ; def f(r):
  (+ r 5)) ;        return r + 5

(define A1 (MyClass 2 3))

>(A1 "r")
7
>(A1 "x")
7
>(A1 "y")
'(3 100 7)
>(A1 "z")
"you are cool"
>((A1 "square") 5)
25

Note: When binding a constructor variable to a list containing constructor variables,
      use (list ...) for list creation instead of '(...)
      ex. (y (list b 100 r)) will give you '(3 100 7)
          whereas
          (y '(b 100 r)) will give you '(b 100 r)

|#
(define-syntax class-constructor
  (syntax-rules ()
    [(class-constructor <Class> (<attr> ...) ; def __init (self, attr1, ...):
                        [(<cAttr> <cBody>) ...] ; constructor variables
                        [(<method> <param> ...) <mBody>] ; built in functions
                        ...)
     (define (<Class> <attr> ...)
       (lambda (msg)
         (let* ([<cAttr> <cBody>] ...) ; bind constructor variables
             (cond [(equal? msg (id->string <attr>)) <attr>] ; gets attribute
                   ...
                   [(equal? msg (id->string <cAttr>)) <cBody>] ; gets constructor attributes
                   ...
                   [(equal? msg (id->string <method>)) ; gets class function
                    (lambda (<param> ...) <mBody>)]
                   ...
                   [else "Unrecognized message!"]))
           ))]))

(define-syntax id->string
  (syntax-rules ()
    [(id->string <id>)
     (symbol->string (quote <id>))]))