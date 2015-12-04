#lang racket

(provide square)
(provide gcd)
(provide abs)
(provide average)



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