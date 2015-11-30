#lang racket
(define (sum-integer a b)
  (if (> a b) 0
      (+ a (sum-integer (+ a 1) b))
      )
  )
(define (cube a) (* a a a))
(define (sum-cube a b)
  (if (> a b) 0
      (+ (cube a) (sum-cube (+ a 1) b))
      )
  )
(define (sum-inverse a b)
  (if (> a b) 0
      (+ (/ 1.0 (* a (+ a 2))) (sum-inverse (+ a 4) b))))

(define (sum-term term a next b)
  (if (> a b) 0
      (+ (term a) (sum-term term (next a) next b))
      )
  )
(define (inc x) (+ x 1))
(define (ident x) x)
(sum-term ident 1 inc 10)
(sum-term cube 1 inc 10)
(sum-cube 1 10)
(* 8 (sum-inverse 1 1000))


(define (integral f a b dx)
  (define (add-dx a) (+ a dx))
  (* (sum-term f (+ a (/ dx 2.0)) add-dx b) dx)
  )
(integral cube 0 1.0 0.0001)

(define 