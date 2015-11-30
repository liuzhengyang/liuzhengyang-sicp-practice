#lang racket
#|
帕斯卡三角形 杨辉三角

|#
(define (pascal-triangle i j)
  (if (or (= j 1) (= i j)) 1
      (+ (pascal-triangle (- i 1) (- j 1)) (pascal-triangle (- i 1) j))
  )
)