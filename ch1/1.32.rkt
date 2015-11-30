#lang racket
(define (accumulate combiner null-value term a next b)
  (if (> a b) null-value
      (combiner (term a) (accumulate combiner null-value term (next a) next b))))
(define (plus a b) (+ a b))
(define (iden x) x)
(define (inc x) (+ x 1))
(define (sum-integer a b)
  (accumulate plus 0 iden a inc b)
  )
(sum-integer 1 100)

(define (multi x y) (* x y))

(define (product term a next b)
  (accumulate multi 1 term a next b)
  )
(define (factorial n)
  (product iden 1 inc n)
  )
(factorial 5)

(define (accululate-it combiner null-value term a next b)
  (define (acc-iter total a)
    (if (> a b) total
        (acc-iter (combiner total (term a)) (next a))
        )
    )
  (acc-iter null-value a)
  )
(define (sum-integer-iter a b)
  (accululate-it plus 0 iden a inc b)
  )
(sum-integer-iter 1 100)
