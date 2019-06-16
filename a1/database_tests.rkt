#| Assignment 1 - Racket Query Language Tests (due Oct 4, 11:50pm on Markus)

***Write the names, CDF accounts and student id for each of your group members below.***
***Max group size is two students.***
Joseph Lee, 1001175346, leejos14
Maher Kazwah, 1001467263, kazwahma
|#

; Note the use of plai here; this is required for "test"
#lang plai
(abridged-test-output #t)

; This imports your file; do not change this line!
(require "database.rkt")

; Test helpers - use these instead of the built-in syntactic forms.

; Please do define And, Or as syntactic forms
; You may use the class code for this.
(define (And x1 x2) (and x1 x2))
(define (AND x1 x2) (And x1 x2))
(define (Or x1 x2) (or x1 x2))
(define (OR x1 x2) (Or x1 x2))
(define (If x1 x2 x3) (if x1 x2 x3))
(define (IF x1 x2 x3) (If x1 x2 x3))

; Sample tables - add more tables!
; Ideas: empty table; table with just 1 attribute; 0 attributes; duplicates
(define Person
  '(("Name" "Age" "LikesChocolate") 
    ("David" 20 #t) 
    ("Jen" 30 #t) 
    ("Paul" 100 #f)))

(define Teaching
  '(("Name" "Course")
    ("David" "CSC324")
    ("Paul" "CSC108")
    ("David" "CSC343")
    ))

(define Egg
  '(("Egg" "Salad")
    ))

(define OneAttribute
  '(("Name")
    ("David")
    ("Paul")
    ("David")
    ))

(define ZeroAttribute
  '(()
    ())
  )

(define DuplicateAttribute
  '(("P1" "P2")
    ("Joseph" "Maher")
    ("Joseph" "Maher")))
              


#|
All tests go below. 
We have divided our tests into five sections:
- No WHERE/ORDER BY
- WHERE clause, but no ORDER BY
- ORDER BY clause, but no WHERE
- Both WHERE and ORDER BY
- Nested queries

Please respect this categorization when you add your tests,
and your TAs will appreciate it!
|#


; ---- SELECT/FROM tests ----
; Select all
(test (SELECT * FROM Person)
      '(("Name" "Age" "LikesChocolate") 
        ("David" 20 #t) 
        ("Jen" 30 #t) 
        ("Paul" 100 #f)))

;Select all from empty table i.e. zero attributes
(test (SELECT * FROM ZeroAttribute)
      '(()()))

;Select all from duplicate attributes
(test (SELECT * FROM DuplicateAttribute)
      '(("P1" "P2")
        ("Joseph" "Maher")
        ("Joseph" "Maher")))

;Select all from duplicate attributes (two tables)
(test (SELECT * FROM [DuplicateAttribute "DA1"] [DuplicateAttribute "DA2"])
      '(("DA1.P1" "DA1.P2" "DA2.P1" "DA2.P2")
        ("Joseph" "Maher" "Joseph" "Maher")
        ("Joseph" "Maher" "Joseph" "Maher")
        ("Joseph" "Maher" "Joseph" "Maher")
        ("Joseph" "Maher" "Joseph" "Maher")))

;Select all from duplicate attributes (three tables one empty)
(test (SELECT * FROM [DuplicateAttribute "DA1"] [DuplicateAttribute "DA2"] [ZeroAttribute "Empty"])
      '(("DA1.P1" "DA1.P2" "DA2.P1" "DA2.P2")
        ("Joseph" "Maher" "Joseph" "Maher")
        ("Joseph" "Maher" "Joseph" "Maher")
        ("Joseph" "Maher" "Joseph" "Maher")
        ("Joseph" "Maher" "Joseph" "Maher")))


; Reordering columns
(test (SELECT '("Age" "LikesChocolate" "Name") FROM Person)
      '(("Age" "LikesChocolate" "Name")
        (20 #t "David") 
        (30 #t "Jen") 
        (100 #f "Paul")))

; Select creates duplicates
(test (SELECT '("Name") FROM Teaching)
      '(("Name")
        ("David")
        ("Paul")
        ("David")))

; Select all from product of table and empty table
(test (SELECT * FROM [Teaching "T"] [ZeroAttribute "Z"])
      '(("Name" "Course")
        ("David" "CSC324")
        ("Paul" "CSC108")
        ("David" "CSC343")
        ))

; Select oneattribute from product of table and empty table
(test (SELECT '("Name") FROM [Teaching "T"] [ZeroAttribute "Z"])
      '(("Name")
        ("David")
        ("Paul")
        ("David")
        ))

; Select given a literal table
(test
 (SELECT '("A" "B")
         FROM '(("C" "A" "B" "D")
                (1 "Hi" 5 #t)
                (2 "Bye" 5 #f)
                (3 "Hi" 10 #t)))
 '(("A" "B")
   ("Hi" 5)
   ("Bye" 5)
   ("Hi" 10)))

;Select all from duplicate literal table
(test
 (SELECT *
         FROM ['(("C" "A" "B" "D")
                 (1 "Hi" 5 #t)
                 (2 "Bye" 5 #f)
                 (3 "Hi" 10 #t)) "T1"]
         ['(("C" "A" "B" "D")
            (1 "Hi" 5 #t)
            (2 "Bye" 5 #f)
            (3 "Hi" 10 #t)) "T2"])
 '(("T1.C" "T1.A" "T1.B" "T1.D" "T2.C" "T2.A" "T2.B" "T2.D")
   (1 "Hi" 5 #t 1 "Hi" 5 #t)
   (1 "Hi" 5 #t 2 "Bye" 5 #f)
   (1 "Hi" 5 #t 3 "Hi" 10 #t)
   
   (2 "Bye" 5 #f 1 "Hi" 5 #t)
   (2 "Bye" 5 #f 2 "Bye" 5 #f)
   (2 "Bye" 5 #f 3 "Hi" 10 #t)
   
   (3 "Hi" 10 #t 1 "Hi" 5 #t)
   (3 "Hi" 10 #t 2 "Bye" 5 #f)
   (3 "Hi" 10 #t 3 "Hi" 10 #t)))


; Select all from two product of two tables
(test (SELECT * FROM [Person "P"] [Teaching "T"])
      '(("P.Name" "Age" "LikesChocolate" "T.Name" "Course")
        ("David" 20 #t "David" "CSC324")
        ("David" 20 #t "Paul" "CSC108")
        ("David" 20 #t "David" "CSC343")
        ("Jen" 30 #t "David" "CSC324")
        ("Jen" 30 #t "Paul" "CSC108")
        ("Jen" 30 #T "David" "CSC343")
        ("Paul" 100 #f "David" "CSC324")
        ("Paul" 100 #f "Paul" "CSC108")
        ("Paul" 100 #f "David" "CSC343")))

; Select all from the product of three tables 
(test (SELECT * FROM [Person "P"] [Teaching "T"] [OneAttribute "O"])
      '(("P.Name" "Age" "LikesChocolate" "T.Name" "Course" "O.Name")
        ("David" 20 #t "David" "CSC324" "David")
        ("David" 20 #t "David" "CSC324" "Paul")
        ("David" 20 #t "David" "CSC324" "David")
        ("David" 20 #t "Paul" "CSC108" "David")
        ("David" 20 #t "Paul" "CSC108" "Paul")
        ("David" 20 #t "Paul" "CSC108" "David")
        ("David" 20 #t "David" "CSC343" "David")
        ("David" 20 #t "David" "CSC343" "Paul")
        ("David" 20 #t "David" "CSC343" "David")
        ("Jen" 30 #t "David" "CSC324" "David")
        ("Jen" 30 #t "David" "CSC324" "Paul")
        ("Jen" 30 #t "David" "CSC324" "David")
        ("Jen" 30 #t "Paul" "CSC108" "David")
        ("Jen" 30 #t "Paul" "CSC108" "Paul")
        ("Jen" 30 #t "Paul" "CSC108" "David")
        ("Jen" 30 #t "David" "CSC343" "David")
        ("Jen" 30 #t "David" "CSC343" "Paul")
        ("Jen" 30 #t "David" "CSC343" "David")
        ("Paul" 100 #f "David" "CSC324" "David")
        ("Paul" 100 #f "David" "CSC324" "Paul")
        ("Paul" 100 #f "David" "CSC324" "David")
        ("Paul" 100 #f "Paul" "CSC108" "David")
        ("Paul" 100 #f "Paul" "CSC108" "Paul")
        ("Paul" 100 #f "Paul" "CSC108" "David")
        ("Paul" 100 #f "David" "CSC343" "David")
        ("Paul" 100 #f "David" "CSC343" "Paul")
        ("Paul" 100 #f "David" "CSC343" "David")))

; Select from two empty tables 
(test (SELECT * FROM [ZeroAttribute "Z1"] [ZeroAttribute "Z2"])
      '(()()))

; Select some from two tables
(test (SELECT '("P.Name" "Course" "Age") FROM [Person "P"] [Teaching "T"])
      '(("P.Name" "Course" "Age")
        ("David" "CSC324" 20)
        ("David" "CSC108" 20)
        ("David" "CSC343" 20)
        ("Jen" "CSC324" 30)
        ("Jen" "CSC108" 30)
        ("Jen" "CSC343" 30)
        ("Paul" "CSC324" 100)
        ("Paul" "CSC108" 100)
        ("Paul" "CSC343" 100)))

;Select some from three tables 
(test (SELECT '("O.Name" "Course" "Age") FROM [Person "P"] [Teaching "T"] [OneAttribute "O"])
      '(("O.Name" "Course" "Age")
        ("David" "CSC324" 20)
        ("Paul" "CSC324" 20)
        ("David" "CSC324" 20)
        ("David" "CSC108" 20)
        ("Paul" "CSC108" 20)
        ("David" "CSC108" 20)
        ("David" "CSC343" 20)
        ("Paul" "CSC343" 20)
        ("David" "CSC343" 20)
        ("David" "CSC324" 30)
        ("Paul" "CSC324" 30)
        ("David" "CSC324" 30)
        ("David" "CSC108" 30)
        ("Paul" "CSC108" 30)
        ("David" "CSC108" 30)
        ("David" "CSC343" 30)
        ("Paul" "CSC343" 30)
        ("David" "CSC343" 30)
        ("David" "CSC324" 100)
        ("Paul" "CSC324" 100)
        ("David" "CSC324" 100)
        ("David" "CSC108" 100)
        ("Paul" "CSC108" 100)
        ("David" "CSC108" 100)
        ("David" "CSC343" 100)
        ("Paul" "CSC343" 100)
        ("David" "CSC343" 100)))

; Take the product of a table with itself
(test (SELECT '("E.Course" "E1.Course") FROM [Teaching "E1"] [Teaching "E"])
      '(("E.Course" "E1.Course")
        ("CSC324" "CSC324")
        ("CSC108" "CSC324")
        ("CSC343" "CSC324")
        ("CSC324" "CSC108")
        ("CSC108" "CSC108")
        ("CSC343" "CSC108")
        ("CSC324" "CSC343")
        ("CSC108" "CSC343")
        ("CSC343" "CSC343")))

; Take the product of a literal table with an identifier
(test
 (SELECT *
         FROM ['(("Age" "A" "Name" "D")
                 (1 "Hi" 5 #t)
                 (2 "Bye" 5 #f)
                 (3 "Hi" 10 #t))
               "T1"]
         [Person "T2"])
 '(("T1.Age" "A" "T1.Name" "D" "T2.Name" "T2.Age" "LikesChocolate")
   (1 "Hi" 5 #t "David" 20 #t)
   (1 "Hi" 5 #t "Jen" 30 #t)
   (1 "Hi" 5 #t "Paul" 100 #f)
   (2 "Bye" 5 #f "David" 20 #t)
   (2 "Bye" 5 #f "Jen" 30 #t)
   (2 "Bye" 5 #f "Paul" 100 #f)
   (3 "Hi" 10 #t "David" 20 #t)
   (3 "Hi" 10 #t "Jen" 30 #t)
   (3 "Hi" 10 #t "Paul" 100 #f)))

; Take the product of a literal table and an empty table
(test
 (SELECT *
         FROM ['(("Age" "A" "Name" "D")
                 (1 "Hi" 5 #t)
                 (2 "Bye" 5 #f)
                 (3 "Hi" 10 #t))
               "T1"]
         [ZeroAttribute "Empty"])
 '(("Age" "A" "Name" "D")
   (1 "Hi" 5 #t)
   (2 "Bye" 5 #f)
   (3 "Hi" 10 #t)))

; ---- WHERE ----
; Attribute as condition, select all
(test (SELECT *
              FROM Person
              WHERE "LikesChocolate")
      '(("Name" "Age" "LikesChocolate")
        ("David" 20 #t)
        ("Jen" 30 #t)))

; Attribute as condition, select subset
(test (SELECT '("LikesChocolate" "Name")
              FROM Person
              WHERE "LikesChocolate")
      '(("LikesChocolate" "Name")
        (#t "David")
        (#t "Jen")))

; Condition as function of one attribute, select all
(test (SELECT *
              FROM Person
              WHERE (< 50 "Age"))
      '(("Name" "Age" "LikesChocolate")
        ("Paul" 100 #f)))

; Condition as function of one attribute, select none
(test (SELECT '()
              FROM Teaching
              WHERE (equal? "Name" "David"))
      '(()
        ()
        ()))

; Constant true condition
(test (SELECT *
              FROM Person
              WHERE #t)
      Person)

; Constant false compound condition
(test (SELECT *
              FROM Person
              WHERE (> (string-length "David") 20))
      '(("Name" "Age" "LikesChocolate")))

; Condition on a literal table
(test (SELECT '("C" "B")
              FROM '(("A" "B" "C") 
                     (1 2 3)
                     (3 10 40)
                     (4 4 4)
                     (2 3 -1))
              WHERE (odd? "A"))
      '(("C" "B")
        (3 2)
        (40 10)))

; Simple condition on joined tables
(test (SELECT *
              FROM [Person "P"] [Teaching "T"]
              WHERE (equal? "P.Name" "T.Name"))
      '(("P.Name" "Age" "LikesChocolate" "T.Name" "Course")
        ("David" 20 #t "David" "CSC324")
        ("David" 20 #t "David" "CSC343")
        ("Paul" 100 #f "Paul" "CSC108")))

; Compound condition on three joined tables
(test (SELECT '("P1.Name" "P1.LikesChocolate" "P.Age" "Course")
              FROM [Person "P"] [Teaching "T"] [Person "P1"]
              WHERE (And "P.LikesChocolate" (equal? "P1.Name" "T.Name")))
      '(("P1.Name" "P1.LikesChocolate" "P.Age" "Course")
        ("David" #t 20 "CSC324")
        ("Paul" #f 20 "CSC108")
        ("David" #t 20 "CSC343")
        ("David" #t 30 "CSC324")
        ("Paul" #f 30 "CSC108")
        ("David" #t 30 "CSC343")))

; Compound condition on four joined tables where one table is empty
(test (SELECT '("P1.Name" "P1.LikesChocolate" "P.Age" "Course")
              FROM [Person "P"] [Teaching "T"] [Person "P1"] [ZeroAttribute "Empty"]
              WHERE (And "P.LikesChocolate" (equal? "P1.Name" "T.Name")))
      '(("P1.Name" "P1.LikesChocolate" "P.Age" "Course")
        ("David" #t 20 "CSC324")
        ("Paul" #f 20 "CSC108")
        ("David" #t 20 "CSC343")
        ("David" #t 30 "CSC324")
        ("Paul" #f 30 "CSC108")
        ("David" #t 30 "CSC343")))


; ---- ORDER BY ----
; Order by attribute
(test (SELECT *
              FROM Person
              ORDER BY "Age")
      '(("Name" "Age" "LikesChocolate")
        ("Paul" 100 #f)
        ("Jen" 30 #t)
        ("David" 20 #t)))

; Order by attribute, not selected
(test (SELECT '("Name")
              FROM Person
              ORDER BY "Age")
      '(("Name")
        ("Paul")
        ("Jen")
        ("David")))

; Order by a function of an attribute
(test (SELECT *
              FROM Person
              ORDER BY (string-length "Name"))
      '(("Name" "Age" "LikesChocolate")
        ("David" 20 #t)
        ("Paul" 100 #f)
        ("Jen" 30 #t)))

; Order with duplicate
(test (SELECT *
              FROM Teaching
              ORDER BY (+ (string-length "Name") (string-length "Course")))
      '(("Name" "Course")
        ("David" "CSC324")
        ("David" "CSC343")
        ("Paul" "CSC108")))

; Order on a literal table
(test (SELECT *
              FROM '(("A" "B" "C") 
                     (1 2 3)
                     (3 10 40)
                     (4 4 4)
                     (2 3 -1))
              ORDER BY "C")
      '(("A" "B" "C")
        (3 10 40)
        (4 4 4)
        (1 2 3)
        (2 3 -1)))

; Order on two tables
(test (SELECT *
              FROM [Person "P"] [Teaching "T"]
              ORDER BY "Age")
      '(("P.Name" "Age" "LikesChocolate" "T.Name" "Course")
        ("Paul" 100 #f "David" "CSC324")
        ("Paul" 100 #f "Paul" "CSC108")
        ("Paul" 100 #f "David" "CSC343")
        ("Jen" 30 #t "David" "CSC324")
        ("Jen" 30 #t "Paul" "CSC108")
        ("Jen" 30 #t "David" "CSC343")
        ("David" 20 #t "David" "CSC324")
        ("David" 20 #t "Paul" "CSC108")
        ("David" 20 #t "David" "CSC343")))

; Order on three tables where one table is empty
(test (SELECT *
              FROM [Person "P"] [Teaching "T"] [ZeroAttribute "Empty"]
              ORDER BY "Age")
      '(("P.Name" "Age" "LikesChocolate" "T.Name" "Course")
        ("Paul" 100 #f "David" "CSC324")
        ("Paul" 100 #f "Paul" "CSC108")
        ("Paul" 100 #f "David" "CSC343")
        ("Jen" 30 #t "David" "CSC324")
        ("Jen" 30 #t "Paul" "CSC108")
        ("Jen" 30 #t "David" "CSC343")
        ("David" 20 #t "David" "CSC324")
        ("David" 20 #t "Paul" "CSC108")
        ("David" 20 #t "David" "CSC343")))


; ---- ORDER BY and WHERE ----
; Use attributes, select all 
(test
 (SELECT * 
         FROM Person 
         WHERE "LikesChocolate" 
         ORDER BY "Age")
 '(("Name" "Age" "LikesChocolate")
   ("Jen" 30 #t)
   ("David" 20 #t)))

; Use attributes, select one unused attribute
(test
 (SELECT '("Name") 
         FROM Person 
         WHERE "LikesChocolate" 
         ORDER BY "Age")
 '(("Name")
   ("Jen")
   ("David")))

; Two joined tables, select all
(test
 (SELECT * 
         FROM [Person "P"] [Teaching "T"] 
         WHERE (equal? "P.Name" "T.Name")
         ORDER BY "Age")
 '(("P.Name" "Age" "LikesChocolate" "T.Name" "Course")
   ("Paul" 100 #f "Paul" "CSC108")
   ("David" 20 #t "David" "CSC324")
   ("David" 20 #t "David" "CSC343")))

; Three joined tables where one table is empty, select all
(test
 (SELECT * 
         FROM [Person "P"] [Teaching "T"] [ZeroAttribute "Empty"]
         WHERE (equal? "P.Name" "T.Name")
         ORDER BY "Age")
 '(("P.Name" "Age" "LikesChocolate" "T.Name" "Course")
   ("Paul" 100 #f "Paul" "CSC108")
   ("David" 20 #t "David" "CSC324")
   ("David" 20 #t "David" "CSC343")))

; Two joined tables, select some attributes
(test
 (SELECT '("P.Name" "Course" "LikesChocolate")
         FROM [Person "P"] [Teaching "T"] 
         WHERE (equal? "P.Name" "T.Name")
         ORDER BY "Age")
 '(("P.Name" "Course" "LikesChocolate")
   ("Paul" "CSC108" #f)
   ("David" "CSC324" #t)
   ("David" "CSC343" #t)))

 
; ---- Nested queries ----
(test
 (SELECT * 
         FROM (SELECT '("Age" "Name") FROM Person))
 '(("Age" "Name")
   (20 "David")
   (30 "Jen")
   (100 "Paul")))

;nested with empty table
(test
 (SELECT * 
         FROM (SELECT '("Age" "Name") FROM [Person "P"] [ZeroAttribute "Empty"]))
 '(("Age" "Name")
   (20 "David")
   (30 "Jen")
   (100 "Paul")))

(test
 (SELECT '("Person.Name" "Course")
         FROM [(SELECT '("Name") FROM Person) "Person"]
         [(SELECT * FROM Teaching WHERE (Or (equal? "Course" "CSC343")
                                            (equal? "Course" "CSC108")))
          "Teaching"])
 '(("Person.Name" "Course")
   ("David" "CSC108")
   ("David" "CSC343")
   ("Jen" "CSC108")
   ("Jen" "CSC343")
   ("Paul" "CSC108")
   ("Paul" "CSC343")))

; Nested with three tables, one is empty table
(test
 (SELECT '("Person.Name" "Course")
         FROM [(SELECT '("Name") FROM Person) "Person"]
         [(SELECT * FROM Teaching WHERE (Or (equal? "Course" "CSC343")
                                            (equal? "Course" "CSC108")))
          "Teaching"] [ZeroAttribute "Empty"])
 '(("Person.Name" "Course")
   ("David" "CSC108")
   ("David" "CSC343")
   ("Jen" "CSC108")
   ("Jen" "CSC343")
   ("Paul" "CSC108")
   ("Paul" "CSC343")))

; Nested query containing a literal
(test
 (SELECT *
         FROM [(SELECT '("A") 
                       FROM '(("A" "B") 
                              (1)
                              (10)))
               "Table1"]
         [(SELECT *
                  FROM '(("C" "A")
                         ("Hi" "Bye")
                         ("Dog" "Cat")
                         ("Red" "Blue")))
          "Table2"]
         WHERE (And (equal? (string-length "Table2.A") 3) (< 0 "Table1.A")))
 '(("Table1.A" "C" "Table2.A")
   (1 "Hi" "Bye")
   (1 "Dog" "Cat")
   (10 "Hi" "Bye")
   (10 "Dog" "Cat")))

(test
 (SELECT *
         FROM [(SELECT '("A") 
                       FROM '(("A" "B") 
                              (1)
                              (10)))
               "Table1"]
         [(SELECT *
                  FROM '(("C" "A")
                         ("Hi" "Bye")
                         ("Dog" "Cat")
                         ("Red" "Blue")))
          "Table2"]
         WHERE (And (equal? (string-length "Table2.A") 3) (< 0 "Table1.A")))
 '(("Table1.A" "C" "Table2.A")
   (1 "Hi" "Bye")
   (1 "Dog" "Cat")
   (10 "Hi" "Bye")
   (10 "Dog" "Cat")))