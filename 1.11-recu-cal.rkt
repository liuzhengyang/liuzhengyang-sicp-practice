#lang racket
;; 递归计算过程
(define (f-recur-cal n)
  (if
   (< n 3)
     n
   (+
    (f (- n 1))
    (* 2 (f (- n 2)))
    (* 3 (f (- n 3)))
   )
  )
)
;; 迭代计算过程
(define (f-iter-cal n)
  (define (f-iter a b c count)
    (if (= count 0)
        c
       (f-iter (+ a (* 2 b) (* 3 c)) a b (- count 1))
    )
  )
  (f-iter 2 1 0 n)
)