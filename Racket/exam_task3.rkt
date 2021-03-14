#lang racket
;task3

(define (concatenate xs ys)
  (cond [ (empty? ys) '() ]
        [ else (append (append (list (car xs)) (list (car ys))) (concatenate (rest xs) (rest ys))) ]))

;(concatenate `(1 2 3 4) `(7 8 9 10))


(define (shuffle xs)
  (helper xs xs))
(define (helper full half)
  (cond
    [ (equal? (length full) (* 2 (length half))) (concatenate full half) ]
    [ else (helper full (rest half)) ]))



;(shuffle '(2 5 1 3 4 7)) ; -> '(2 3 5 4 1 7)
;(shuffle '(1 2 3 4 4 3 2 1)) ; -> '(1 4 2 3 3 2 4 1)
;(shuffle '(1 1 2 2)) ; -> '(1 2 1 2)