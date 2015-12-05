#lang racket


(define x '(1 2 3))
(define y '(4 5 6))

(list x y)
(car (list x y))
(car (cdr (list x y)))
(append x y)
(cons x y)