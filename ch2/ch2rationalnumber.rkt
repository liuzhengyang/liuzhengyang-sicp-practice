#lang racket

(require "../commons.rkt")
(cdr (cdr '(1 2)))

(define x (cons 1 2))
(define y (cons 3 4))
(define z (cons x y))
(car (car z))
(car (cdr z))


(define (make-rat n d)
  (define neg
    (or (and (> n 0) (< d 0)) (and (< n 0) (> d 0))))
  
  (define gcd-nd (abs (gcd n d)))
  (define flag (if neg -1 1))
  
  (cons
   (/ (* flag (abs n)) gcd-nd)
   (/ (abs d) gcd-nd)))

(define (numer x) (car x))
(define (denom x) (cdr x))

(define (print-rat x)
  (newline)
  (display (numer x))
  (display "/")
  (display (denom x)))

(print-rat (make-rat 2 3))
(define one-half (make-rat 1 2))
(print-rat one-half)



(define (add-rat a b)
  (make-rat (+ (* (numer a) (denom b))
               (* (denom a) (numer b)))
            (* (denom a) (denom b)))
  )
(define (mul-rat x y)
  (make-rat
   (* (numer x) (numer y))
   (* (denom x) (denom y))
   ))
(define (sub-rat a b)
  (make-rat (- (* (numer a) (denom b))
               (* (denom a) (numer b)))
            (* (denom a) (denom b))))
(define (div-rat x y)
  (make-rat (* (numer x) (denom y)) (* (denom x) (numer y))))

(define (equal-rat? x y)
  (= (* (numer x) (denom y))
     (* (numer y) (denom x))))

(print-rat (add-rat (make-rat 2 3) (make-rat 3 4)))

(print-rat (sub-rat (make-rat 2 3) (make-rat 1 2)))

(print-rat (mul-rat (make-rat 2 3) (make-rat 1 2)))

(print-rat (div-rat (make-rat 2 3) (make-rat 1 2)))


(equal-rat? (make-rat 1 2) (make-rat 2 4))


(make-rat 2 -4)


