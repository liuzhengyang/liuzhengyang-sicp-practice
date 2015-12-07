#lang racket

(require "../commons.rkt")

(define (dot-product v w)
  (accumulate + 0 (map * v w)))

(define (matrix-*-vector m v)
  (map (lambda (row) (dot-product row v)) m))

(define (transpose m)
  (accumulate-n cons nil m))


(define v (list 1 2 3 5))
(define w (list 2 3 4 5))
(define x '((1 2 3 4) (4 5 6 6) (6 7 8 9)))

(dot-product v w)
(matrix-*-vector (list v v v w) w)
(transpose (list v v))
(transpose x)

(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
    (map (lambda (row) (matrix-*-vector cols row)) m)))

(matrix-*-matrix x '((1 2) (1 3) (2 4) (1 0)))
