#lang racket

(require "../commons.rkt")
  
(define (count-leaves t)
  (accumulate + 0 (map (lambda (x) (if (not (pair? x)) 1 (count-leaves x))) t)))

(define x (list 1 (list 2 2) (list 3 4)))

(count-leaves x)
(count-leaves (list x x))

