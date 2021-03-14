#lang racket

;задача 1

(define (square x)
  (* x x)
  )


(define (automorphic? n)
  (define (helper? a b) ;сравнява двата остатъка
    (cond
      [(= a 0) #t] ;ако остатъка е равен на 0
      [(= (remainder a 10) (remainder b 10)) (helper? (quotient a 10) (quotient b 10))] ;ако остатъците са еднакви , викаме helper с числата делени на 10
      [else #f] ;ако остатъците не са еднакви
      )
    )
  (helper? n (square n))
  )

(automorphic? 5)
(automorphic? 25)
(automorphic? 36)
(automorphic? 890625)


;задача 2

(define (prime? n)
  (define (helper current)
    (if (> current (/ n 2))
        #t
        (and (not (= (remainder n current) 0)) (helper (+ current 1)))))
  (if (<= n 1)
      #f
      (helper 2)))
 
(define (nth-cuban n)
    (define (helper a b n)
        (cond 
           [(and (= n 1) (prime? (- (* b b b) (* a a a)))) (- (* b b b) (* a a a))] ;при n = 1, и b^3 - a^3 просто число
           [(and (> n 1) (prime? (- (* b b b) (* a a a)))) (helper (+ a 1) (+ b 1) (- n 1))] ;при n > 1 и b^3 - a^3 просто число , викаме helper със следващата двойка числа и намаляме n с 1
           [else (helper (+ a 1) (+ b 1) n)] 
         )
 
    ) (helper 1 2 n)
)


(nth-cuban 1) 
(nth-cuban 4) 
(nth-cuban 50) 
(nth-cuban 100)
