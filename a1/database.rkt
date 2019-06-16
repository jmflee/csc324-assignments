#| Assignment 1 - Racket Query Language  (due Oct 4, 11:50pm, on Markus)

***Write the names, CDF accounts and student IDs for each of your group members below.***
***Max group size is two students.***
Joseph Lee, 1001175346, leejos14
Maher Kazwah, 1001467263, kazwahma
|#
#lang racket

; TODO: After you have defined your macro(s), make sure you add each one
; to the provide statement.
(provide attributes
         tuples
         size
         And
         AND
         Or
         OR
         If
         IF
         replace
         where
         order
         rho
         SELECT)

; Part 0: Semantic aliases

; SQL syntax
(define (And x1 x2) (and x1 x2))
(define (AND x1 x2) (And x1 x2))
(define (Or x1 x2) (or x1 x2))
(define (OR x1 x2) (Or x1 x2))
(define (If x1 x2 x3) (if x1 x2 x3))
(define (IF x1 x2 x3) (If x1 x2 x3))

#|
(attributes table)
  table: a valid table (i.e., a list of lists as specified by assigment)

  Returns a list of the attributes in 'table', in the order they appear.
|#
(define (attributes lst)
  (if (null? lst)
      '()
      (first lst)))

#|
(tuples table)
  table: a valid table

  Returns a list of all tuples in 'table', in the order they appear.
  Note: it is possible for 'table' to contain no tuples.
|#
(define (tuples lst)
  (if (null? lst)
      '()
      (rest lst)))

#|
(size table)
  table: a valid table

  Returns the number of tuples in 'table'.
|#
(define (size lst)
  (if (null? lst)
      0
      (length lst)))


; Part I "WHERE" helpers; you may or may not wish to implement these.

#|
'table-searchs' takes: 
 aLst - a list of attributes
 a - a string (representing an attribute)
 tups - list of tuples
 count - keeps track of the list-ref location of the attribute

 and returns all the values of a table corresponding to that attribute.
|#
(define (table-searchs aLst a tups [count 0])
  (cond [(null? a)
         (build-list (size tups) (const '()))]
        [(equal? a (first aLst))
         (my-map tups count)]
        [(table-searchs (rest aLst) a tups (+ count 1))]))

#|
'table-search' takes:
 aLst - a list of attributes
 a - a string attribute
 tups - a singular tuple

 returns the attribute value in a tuple
|#
(define (table-search aLst a tups [count 0])
  (if (null? aLst)
      a
      (if (equal? a (first aLst))
          (list-ref tups count)
          (table-search (rest aLst) a tups (+ count 1)))))

#|
'my-map' takes:
 tups - a list of tuples
 count - a int inficating the list-ref location of the attribute

 returns a list of values correlating to a tuples' list-ref location
|#
(define (my-map tups count)
  (if (null? tups)
      '()
      (append (list (list-ref (car tups) count)) (my-map (cdr tups) count))))

#|
'table-filter' takes:
 f - list of lists of lists of ... of boolean values
 table - a table

  and returns a new table containing only the tuples in 'table'
  that satisfy the boolean values.
|#
(define (table-filter f table)
  (cons (attributes table) (filter f (tuples table))))

#|
'replace-attr' takes:
 a - a string of an attributes
 aLst - a list of attributes

  and returns a function 'f' which takes a tuple and does the following:
    - If 'x' is in the list of attributes, return the corrresponding value 
      in the tuple.
    - Otherwise, just ignore the tuple and return 'x'.
|#
(define (replace-attr x aLst)
  (λ(tup)
    (if (member x aLst)
        (table-search aLst x tup)
        x)))

#|
'cartese-allt' takes:
 uTuples - a gigantic list of list of ... tuples
 newTups - new table to be returned

 returns a cartesian product of all the tubles in uTups

>(cartese-allt '((tuples Person) (tuples Person) '((3))))
'(("David" 20 #t "David" 20 #t 3)
  ("David" 20 #t "Jen" 30 #t 3)
  ("David" 20 #t "Paul" 100 #f 3)
  ("Jen" 30 #t "David" 20 #t 3)
  ("Jen" 30 #t "Jen" 30 #t 3)
  ("Jen" 30 #t "Paul" 100 #f 3)
  ("Paul" 100 #f "David" 20 #t 3)
  ("Paul" 100 #f "Jen" 30 #t 3)
  ("Paul" 100 #f "Paul" 100 #f 3))
|#
(define (cartese-allt uTuples [newTups '()])
  (cond [(and (null? uTuples) (null? newTups))
         '()]
        [(null? newTups)
         (cartese-allt (rest uTuples) (first uTuples))]
        [(null? uTuples)
         newTups]
        [(cartese-allt (rest uTuples) (cartesian-product newTups (first uTuples)))]))

#|
'cartese-alla' takes:
 uAttr - a list of list of attributes
 rName - a list of names for renaming a table's attributes

 return a list of all the attributes in uALst and renames their respective attributes
 if their corresponding rName is not equal to 7

>(cartese-alla '(("Name" "Age" "LikesChocolate") '("A" "B" "C") '(3)) '(7 7 7))
'("Name" "Age" "LikesChocolate" "A" "B" "C" 3)

>(cartese-alla '(("A" "B" "C") '("A" "B" "C") '("1" "2"))
'("1.A" "1.B" "1.C" "2.A" "2.B" "2.C")

|#
(define (cartese-alla uAttr rName)
  (let ([allAttr (apply append uAttr)])
    (apply append (attrs-rename uAttr allAttr rName))
    ))

#|
'attrs-rename' takes:
 uAttr - a list of list of attributes
 allAttr - master list of attributes
 rName - a list of names for renaming a table's attributes
 count - a counter to snip off part of allAttr
 rhod - new list of attributes with rename'd attributes

 returns rhod, a list of attributes with the rho rename operation
|#
(define (attrs-rename uAttr allAttr rName [count 0] [rhod '()])
  (if (null? uAttr)
      rhod
      (let ([aLst (first uAttr)])
        (let ([rLst (append (take allAttr count) (drop allAttr (+ count (length aLst))))])
          (attrs-rename (rest uAttr) allAttr (rest rName) (+ count (length aLst)) (append rhod (list (attr-rename aLst rLst (first rName))))))
        )))

#|
'attr-rename' takes:
 aLst - a list of attributes
 allAttr - a list of combined table attributes
 r - the particular name that the table/attributes will be renamed to
 rhod - a portion of the rename'd attributes relative to aLst

 returns renamed attributes for one table
|#
(define (attr-rename aLst allAttr r [rhod '()])
  (cond [(null? aLst)
         rhod]
        [(equal? r 7)
         (attr-rename (rest aLst) allAttr r (append rhod (list (first aLst))))]
        [(member (first aLst) allAttr)
         (attr-rename (rest aLst) allAttr r (append rhod (list (string-append r "." (first aLst)))))]
        [(attr-rename (rest aLst) allAttr r (append rhod (list (first aLst))))]
        ))

; Starter for Part 3; feel free to ignore!

; Recursive macro that converts (replace (> "Age" 10) Person) to '(#<procedure:>> num 10)
(define-syntax replace
  (syntax-rules ()
    ; The recursive step, when given a compound expression
    [(replace (expr ...) table)
     ; Change this!
     (λ(tup)
       (let ([sum (list ((replace expr table) tup) ...)])
         (apply (first sum) (rest sum))
         ))]
    ; The base case, when given just an atom. This is easier!
    [(replace atom table)
     ; Change this!
     (λ(tup)
       ((replace-attr atom (attributes table)) tup))]
    ))

#|
'where' take:
 <expr> - an expression usually in the form of (> "Age" 40) or something similar
 <table> - any table

 returns a table adhering to the conditions set by <expr>
|#
(define-syntax where
  (syntax-rules()
    [(where <expr> <table>)
     (let ([sum (replace <expr> <table>)])
       (table-filter sum <table>)
       )]))

#|
'order' takes:
 <expr> - an expression
 <table> - a table

 returns a sorted table adhering to the conditions set by <expr>
|#
(define-syntax order
  (syntax-rules()
    [(order <expr> <table>)
     (cons (attributes <table>)
           (sort (tuples <table>)
                 >
                 #:key (replace <expr> <table>)))]
    ))

#|
'shuffle' takes:
 aLst - a list of attributes
 table - a table

 returns a new table with only the attributes in aLst
|#
(define (shuffle aLst table)
  (cond [(null? table)
         table]
        [(null? aLst)
         (map (λ(n) '()) table)]
        [(cons aLst (shuffle-fix (map (λ(x) (table-searchs (attributes table) x (tuples table))) aLst)))]))

#|
'shuffle-fix' takes:
 table - a table
 fixed-table - a modified table showing the attributes in the correct order
 pos - book keeping counter to keep track of which tuple is being modified

 returns a set of transformed tuples
|#
(define (shuffle-fix table [fixed-table '()] [pos 0])
  (if (equal? (length (attributes table)) pos)
      fixed-table
      (shuffle-fix table (append fixed-table (list (my-map table pos))) (+ pos 1))))

#|
(define (process iLst)
  (cond [(equal? (size iLst) 3)
         (map (λ(val) ((first iLst) val (third iLst))) (second iLst))]
        [(equal? (size iLst) 2)
         (map (first iLst) (rest iLst))]
        [iLst]))
|#

#|
cartesian-product takes two arguments:
 t1 - table 1 a list of lists
 t2 - table 2 a list of lists

 Returns an empty list if either t1 or t2 are empty
 Otherwise, it should return the cartesian product of two tables/tubles
|#
(define (cartesian-product t1 t2)
  (cond [(null? t1) '()]
        [(null? t2) '()]
        [(append (map (λ(t) (append (car t1) t)) t2) (cartesian-product (rest t1) t2))]
        ))

#|
'rho' takes:
 <t1> - a table
 <n1> - a new name for the table
 ...
 <tn> - some table which may or may not contain a new name

 Returns a table paired with a new name <n1> otherwise will be paired with 7
|#
(define-syntax rho
  (syntax-rules ()
    [(rho [<t1> <n1>])
     (cons <t1> <n1>)]
    [(rho <t1>)
     (cons <t1> 7)]
    [(rho <t1> ...)
     (append (list (rho <t1>)) ...)]))

#|
'combine' takes:
 uLst - a list of tables that have been rho'd

 returns the cartesian product of all the tables
|#
(define (combine uLst)
  (if (null? uLst)
      uLst
      (let ([rName (map (λ(table) (cdr table)) uLst)])
        (let ([table (map (λ(table) (car table)) uLst)])
          (let ([allAttr (map attributes table)])
            (let ([allTups (map tuples table)])
              (cons (cartese-alla allAttr rName) (cartese-allt allTups))))))))

(define-syntax SELECT
  (syntax-rules (* FROM WHERE ORDER BY)
    [(SELECT * FROM <t1>)
     <t1>]
    [(SELECT <aLst> FROM <t1> ... WHERE <cond> ORDER BY <order>)
     (SELECT <aLst> FROM
             (order <order>
                    (where <cond> 
                           (SELECT * FROM <t1> ...))))]
    
    [(SELECT <aLst> FROM <t1> ... ORDER BY <order>)
     (SELECT <aLst> FROM
             (order <order>
                    (SELECT * FROM <t1> ...)))]
    
    [(SELECT <aLst> FROM <t1> ... WHERE <cond>)
     (SELECT <aLst> FROM
             (where <cond>
                    (SELECT * FROM <t1> ...)))]
    
    [(SELECT <aLst> FROM <t1>)
     (shuffle <aLst> (SELECT * FROM <t1>))]
    
    [(SELECT * FROM <t1> <t2> ...)
     (combine (rho <t1> <t2> ...))]
    
    [(SELECT <aLst> FROM <t1> <t2> ...)
     (shuffle <aLst> (SELECT * FROM <t1> <t2> ...))]
    
    ))