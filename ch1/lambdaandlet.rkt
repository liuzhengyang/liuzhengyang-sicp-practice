#lang racket

(require "ch1prime.rkt")


(prime? 10)

((lambda (x) (+ x 4)) 4)
(define (square x) (* x x))




(define (f x y)
  (let ((a (+ 1 (* x y)))
        (b (- 1 y)))
    (+ (* x (square a))
       (* y b)
       (* a b)
       )
    )
  )
(f 1 0)