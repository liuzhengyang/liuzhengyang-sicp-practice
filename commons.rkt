#lang racket

(provide square)
(provide gcd)
(provide abs)
(provide average)
(provide base-exp)
(provide accumulate)
(provide accumulate-n)
(provide nil)
(provide prime?)
(provide flatmap)
(provide filter)
(provide enumerate-interval)

(define nil '())



;; 平方
(define (square x) (* x x))

;; 最大公约数
(define (gcd a b)
  (if (= b 0) a
      (gcd b (remainder a b))))

;; 绝对值
(define (abs x)
  (if (< x 0) (- x)
    x))

;; 平均值
(define (average a b)
  (/ (+ a b) 2)
  )

;; 求幂
(define (base-exp base n)
  (cond ((= n 0) 1)
        ((odd? n) (* base (base-exp base (- n 1))))
        (else (square (base-exp base (/ n 2))))))


(define tolerance 0.00001)
(define (close? a b)
  (< (abs (- a b)) tolerance))

(define (damp f)
  (lambda (x) (average x (f x))))
(define (fix-point f)
  (define (fix-point1 f guess)
    (if (close? guess (f guess)) guess
        (fix-point1 f (f guess))))
  (fix-point1 (damp f) 1.0)
  )


(define (sqrt x)
  (fix-point (lambda (y) (/ x y))))

(define (accumulate op initial lst)
  (if (null? lst) initial
      (op (car lst) (accumulate op initial (cdr lst)))))

(define (accumulate-n op init lst)
  (if (null? (car lst)) nil
      (cons (accumulate op init (map car lst))
            (accumulate-n op init (map cdr lst)))))

(define (prime? n)
  (define (prime-iter i)
    (cond ((> (square i) n) true)
          ((= (remainder n i) 0) false)
          (else (prime-iter (+ i 1)))))
  (prime-iter 2))

(define (flatmap proc seq)
  (accumulate append nil (map proc seq)))

(define (filter preconditions? seq)
  (cond ((null? seq) nil)
      ((preconditions? (car seq)) (cons (car seq) (filter preconditions? (cdr seq))))
      (else (filter preconditions? (cdr seq)))))

(define (enumerate-interval low high)
  (if (> low high) nil
      (cons low (enumerate-interval (+ low 1) high))))
