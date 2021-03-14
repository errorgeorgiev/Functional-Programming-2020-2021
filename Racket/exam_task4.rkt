#lang racket
;task 4


(define (check-lst lst n)
  (define (helper element lst n)
    (cond
      [ (= n 0) #t ]
      [ (not(= element 0)) #f]
      [ else (helper (car (cdr lst)) (cdr lst) (- n 1))]))
  (helper (car lst) lst n))



;(check-lst `(3 2 0 4 5) 0)
;(check-lst `(0 0 0 4 5) 2)

(define (triangular? mat)
  (define (helper mat counter)
    (cond
      [ (null? mat) #t ]
      [ (not(check-lst (car mat) counter)) #f ]
      [ else (helper (cdr mat) (+ counter 1)) ]))
  (helper mat 0))



  
(triangular? '((1 2 3)
 (0 5 6)
 (0 0 9))) ; -> #t
(triangular? '((0 2 3)
 (0 0 6)
 (1 0 0))) ; -> #f
(triangular? '((1 2 3)
 (1 5 6)
 (0 0 9))) ; -> #f
(triangular? '((1 2 3 4)
 (0 5 6 7)
 (0 0 8 9)
 (0 0 0 9))) ; -> #t

