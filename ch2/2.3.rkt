#lang racket
(require "../commons.rkt")
(require "2.2.rkt")


(define (sqrt-sum-square x y)
  (sqrt (+ (square x) (square y))))

(define (length l)
  (sqrt-sum-square (- (x-point (start-segment l)) (x-point (end-segment l)))
                         (- (y-point (start-segment l)) (y-point (end-segment l)))))
;; a and d are two conjointed lines
(define (make-rectangle a d)
  (cons a d))

(define (a-line r)
  (car r))
(define (b-line r)
  (cdr r))

;(define (perimete r)
(length (make-segment (make-point 1 2) (make-point 2 3)))

(define (perimeter r)
  (* 2 (+ (length (a-line r)) (length (b-line r)))))

(define (area r)
  (* (length (a-line r)) (length (b-line r))))


(define rect (make-rectangle (make-segment (make-point 1 2) (make-point 1 4)) (make-segment (make-point 1 2) (make-point 2 2))))
(area rect)
(perimeter rect)