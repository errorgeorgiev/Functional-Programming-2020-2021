#lang racket
;task2
#|
Задача 2. Да се дефинира процедура (kth-max-min xs), която приема списък
от цели числа и връща процедура с параметър естествено число k – такава, че
оценката на израза ((kth-max-min xs) k) e k-тото по големина отрицателно
число в xs. Ако такова число не съществува, да се връща грешката “No such
number”.
|#



(define (kth-max-min xs)
  (λ (k)
    (define (helper xs k)
      (cond
        [ (not(and (= k 0) (< (car (sort xs <)) 0))) error "no such number" ]
        [ (and (= k 0) (< (car (sort xs <)) 0)) (car (sort xs <)) ]
        [ else (helper (cdr (sort xs <)) (- k 1)) ]
        
        ))
    (helper xs (- k 1))))

((kth-max-min '(1 2 3 4 -5 6 7 -2 -1 0)) 2) ; -> -2
((kth-max-min '(-1 0 -1 0 -2 3 1 -1)) 3) ; -> No such number


    