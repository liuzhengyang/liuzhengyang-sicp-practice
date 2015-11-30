#lang racket
(define (product term a next b)
  (if (> a b) 1.0
      (* (term a) (product term (next a) next b))))

(define (square x) ( * x x))

(define (two-divide-three a)
  (/ (* 4 (+ a 1) a) (square (+ (* 2 a) 1)))
  )
(define (inc n) (+ n 1))

(* 4 (product two-divide-three 1 inc 10000))

(define (product-iter-cal term a next b)
  (define (product-iter product a)
    (if (> a b) product
        (product-iter (* product (term a)) (next a))))
  (product-iter 1.0 a)
  )
(* 4 (product-iter-cal two-divide-three 1 inc 10000))