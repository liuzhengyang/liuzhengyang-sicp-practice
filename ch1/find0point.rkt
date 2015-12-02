#lang racket

; 函数零点
(define (average a b) (/ (+ a b) 2))
(define (square x) (* x x))
(define (abs a) (if (> a 0) a (- a)))
(define (close? a b) (< (abs (- a b)) 0.001))
(define (negative? x) (< x 0))
(define (positive? x) (> x 0))
(define (point0 f neg pos)
  (let ((mid (average neg pos))
         )
     (cond ((close? neg pos) mid)
           ((negative? (f mid)) (point0 f mid pos))
           ((positive? (f mid)) (point0 f neg mid))
           (else mid)
           )
     )
  )
(define (half-interval-method f a b)
  (let ((a-value (f a))
        (b-value (f b)))
    (cond ((and (negative? a-value) (positive? b-value))
           (point0 f a b))
          ((and (positive? a-value) (negative? b-value))
           (point0 f b a))
          (else (error "Value are not of opposite sign" a b))
          )
    )
  )
         
    
(point0 (lambda (x) (* x x x)) -10.0 4)
(half-interval-method sin 2.0 4.0)
(half-interval-method (lambda (x) (+ (* x x x) (- (* 2 x)) (- 3))) 1.0 2.0)


;; 函数不动点
(define tolerance 0.00001)
(define (close-fix? a b) (< (abs (- a b)) tolerance))
(define (fix-point f first-guess)
  (if (close-fix? first-guess (f first-guess))
      first-guess
      (fix-point f (f first-guess))
      )
  )
(fix-point cos 1.0)

; 利用不用点求sqrt y = x/y  y = (x/y + y)/2
(define (new-sqrt x)
  (fix-point (lambda (y) (/ (+ (/ x y) y) 2)) 1.0)
  )

(new-sqrt 2)

(fix-point (lambda (x) (+ 1 (/ 1 x))) 1.0)