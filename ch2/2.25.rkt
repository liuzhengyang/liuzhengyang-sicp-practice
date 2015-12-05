#lang racket

(define l1 '(1 3 (5 7) 9))
(car (cdr (car (cdr (cdr l1)))))

(define l2 '((7)))
(car (car l2))

(define l3 '(1 (2 (3 (4 (5 (6 7)))))))
(define (repeat-get l count)
  (if (= count 0) (car l)
      (repeat-get (car (cdr l)) (- count 1))))

(car (cdr (car (cdr (car (cdr (car (cdr (car (cdr (car (cdr l3))))))))))))

(define (list-ref l n)
  (if (= n 0) (car l)
      (list-ref (cdr l) (- n 1))))