#lang racket
;task1
(define (num-of-digits n)
  (cond
    [(= n 0) 0 ]
    [ else (+ 1 (num-of-digits(quotient n 10)))]))


(define (get-sum n currentPower)
  (cond
    [ (= n 0) 0 ]
    [ else (+ (expt (remainder n 10) currentPower) (get-sum(quotient n 10) (+ currentPower 1)))]))

(get-sum 89 1)
(get-sum 695 1)


