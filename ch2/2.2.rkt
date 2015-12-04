#lang racket

(require "../commons.rkt")


(define (make-point x y) (cons x y))

(define (x-point p)
  (car p)
  )
(define (y-point p)
  (cdr p)
  )

(define (make-segment p1 p2)
  (cons p1 p2)
  )
(define (start-segment l)
  (car l)
  )
(define (end-segment l)
  (cdr l)
  )


(define (midpoint-segment l)
  (make-point
   (average (x-point (start-segment l)) (x-point (end-segment l)))
   (average (y-point (start-segment l)) (y-point (end-segment l)))
   )
  )

(define (print-point p)
  (newline)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")"))


(print-point (make-point 1 2))
(print-point (make-point 1 2))

(print-point (midpoint-segment (make-segment (make-point 1 4) (make-point 2 9))))