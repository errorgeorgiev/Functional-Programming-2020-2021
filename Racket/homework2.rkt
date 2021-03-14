#lang racket

;task1

(define (cartesian-product xs ys)
  (define (helper xs ys copy)
    (cond
      [ (null? xs) `() ]
      [ (null? ys) (helper (cdr xs) copy copy) ]
      [ else (cons (cons (car xs) (car ys)) (helper xs (cdr ys) copy)) ]
      ))
  (helper xs ys ys)
)

(cartesian-product '(1 2) '(3 4)) 
(cartesian-product '(1 2 3 4 5) '(6 7 8)) 


;task2
;checks if a number is prime 
(define (is-prime? n)
  (define (helper current)
    (if (> current (/ n 2))
        #t
        (and (not (= (remainder n current) 0)) (helper (+ current 1)))))
  (if (<= n 1)
      #f
      (helper 2)))

;(is-prime? 2)
;(is-prime? 7)
;(is-prime? 22)


;finds next prime number
(define (find-next-prime n)
  (cond
    [ (is-prime? (+ n 1)) (+ n 1) ]
    [ else (find-next-prime (+ n 1)) ]))

;(find-next-prime 2)
;(find-next-prime 3)
;(find-next-prime 31)
;(find-next-prime 16)




;factorizing func
(define (factorize n)
  (define (helper current n)
    (cond
      [ (is-prime? n) (list n) ]
      [ (not(= (remainder n current) 0)) (helper (find-next-prime current) n) ]
      [ (= (remainder n current) 0)  (cons current (helper 2 (quotient n current))) ]))
  (helper 2 n))


(factorize 6) ; -> '(2 3)
(factorize 13) ; -> '(13)
(factorize 123) ; -> '(3 41)
(factorize 152) ; -> '(2 2 2 19)