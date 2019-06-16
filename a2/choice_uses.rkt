#| Assignment 2 - Using Backtracking

This file contains starter code for questions 4-6,
which involve using backtracking in interesting ways, and
extending the functionality of the backtracking library.
|#
#lang racket

; Import choice API
(require "choice.rkt")

; Export functions for testing. Please don't change this line!
(provide subsets sudoku-4 fold-<)

; QUESTION 3
#|
(subsets lst)
  lst: a list

  A choice expression which yields a subset of 'lst'.
  Repeated calls to 'next' should yield *all* other subsets
  (i.e., 'next' returns "false." only when all subsets of 'lst'
  have been returned).

  The subsets can be yielded in any order; however, no subset
  can appear twice.

  Note that:
    - A subset isn't the same as a sublist. Items don't have to be consecutive.
    - A subset can be empty; the empty set is a subset of any list.
    - Order doesn't matter in subsets
  
  The following is an example of how 'subsets' is used.
  Note that your implementation might yield the subsets
  in a different order than the one shown here.

> (subsets '(1 2))
'()
> (next)
'(1)
> (next)
'(2)
> (next)
'(1 2)
> (next)
"false."
|#
(define (subsets lst)
  (all-< (powerset lst)))


#|
(add-to-all lst item)
lst - a list of anything
item - item to add to the list
Combines every element in lst with item and returns a list of lists from it
|#
(define (add-to-all lst item)
  (if (equal? lst null)
      '()
      (cons (cons item (car lst))
            (add-to-all (cdr lst) item))))
#|
(powerset lst)
lst - a lst of anything
Gets a list and returns the powerset of the list
|#
(define (powerset lst)
  (if (equal? lst null)
      '(())
      (let ((all (powerset (cdr lst))))
            (append (add-to-all all (car lst)) all))))

#|
(all-< lst)
lst - a list of anything
Gets a list and shoves every element into the stack
|#
(define (all-< lst)
  (cond [(empty? lst) lst]
        [(= (length lst) 1) (first lst)]
        [(-< (first lst) (all-< (rest lst)))]))

; QUESTION 4
#|
(sudoku-4 puzzle)
  puzzle: a nested list representing a 4-by-4 Sudoku puzzle

  A choice expression that represents possible solutions to the puzzle.
  Upon evaluation, just one solution is returned, but repeated calls
  to 'next' produces more possible solutions, if any.

  Hint: use the ?- function in your solution. Again, your main task
  is just to correctly express the constraints, and let the computer
  do the work.
|#
(define (sudoku-4 puzzle)
  (let ([all-combos (all-< (get-combinations puzzle))])
    (?- valid-board? all-combos)))

#|
(get-combinations puzzle [combos])
puzzle - a list of lists (4x4 sudoku grid)
combos - a list of list of list of all possible combinations that the ""
         can be filled with in the sudoku grid
Gets a sudoku puzzle and returns a list of sudoku grids containing every
single combination that the "" could be filled with
|#
(define (get-combinations puzzle [combos '()])
  (cond [(empty? puzzle)
         combos] ; The finished product, all boards of possible sudoku grids
        [(let* ([possibilities (permutations (get-fill (first puzzle)))] ; Get the list of possible inputs for one row
                [row-combos (map (curry get-combos (first puzzle)) possibilities)]) ; Get the list of possible rows in respect to the current row
           (get-combinations (rest puzzle)
                             (cond [(and (empty? combos) (member "" (first puzzle))) ; First row is unfilled
                                    (map (λ(row) (list row)) row-combos)]
                                   [(empty? combos) ; First row is not unfilled
                                    (list (list (first puzzle)))]
                                   [(member "" (first puzzle)) ; Row is unfilled but NOT the first row
                                    (cartesian-product combos row-combos)]
                                   [(cartesian-product combos (list (first puzzle)))])))]))

#|
(cartesian-product lst1 lst2)
lst1 - first list (can be list of lists or just a list)
lst2 - second list (can be list of lists or just a list)
Gets the cartesian product between two lists
|#
(define (cartesian-product lst1 lst2)
  (if (empty? lst1)
      '()
      (append (map (λ(row) (append (first lst1) (list row))) lst2)
              (cartesian-product (rest lst1) lst2))))

#|
(get-combos row fill)
row - row from the sudoku grid (a list)
fill - possible values that can be inserted into the "" (a list)
Gets a sudoku row and fills the "" parts with the elements in fill
|#
(define (get-combos row fill)
  (cond [(empty? row)
         '()]
        [(equal? "" (first row))
         (cons (first fill) (get-combos (rest row) (rest fill)))]
        [(cons (first row) (get-combos (rest row) fill))]))

#|
(get-fill row [fill '(1 2 3 4)])
row - row from the sudoku grid (a list)
fill - list of possible values in 4x4 sudoku ie. '(1 2 3 4)
Gets a row in sudoku and returns a list of values that it is missing
|#
(define (get-fill row [fill '(1 2 3 4)])
  (filter (λ(x) (not (member x row))) fill))

#|
(valid-board? puzzle)
puzzle - a sudoku grid (list of lists)
Returns true or false depending if a filled in sudoku grid is valid or not
|#
(define (valid-board? puzzle)
  (andmap (λ(x) (and #t x)) (map are-all-unique? (rotate puzzle))))

(define (are-all-unique? v)
  (if (pair? v)
      (and (not (member (car v) (cdr v)))
           (are-all-unique? (cdr v)))
      #t))

#|
(rotate puzzle [x])
puzzle - a sudoku grid (list of lists)
x - a counter for list-ref to keep track of where the list is
Gets a filled in sudoku grid and rotates the values 90 degrees clockwise
|#
(define (rotate puzzle [x 0])
  (if (= x (length puzzle))
      '()
      (cons (map (curryr list-ref x) puzzle) (rotate puzzle (+ x 1)))))

; QUESTION 5
#|
(fold-< combine init expr)
  combine: a binary function
  init: an initial value
  expr: a choice expression

  Evaluate all choices in <expr> and combine them, one at a time, with the
  initial value, and return the result.

  Note that the order of <combine>'s parameters is the same as foldl:
    1) The value of the next choice
    2) The value of <init>
|#
(define-syntax fold-<
  (syntax-rules ()
    [(fold-< <combine> <init> <expr>)
     ; fold the expression with all
     (foldl <combine> <init> (all <expr>))]))