#lang racket
(define (sqrt x)

  (define (square x)
    (* x x)
    )
             
  (define (good? guess)
    (< (abs (/ (- (square guess) x) x)) 0.001)
    )
  (define (improve guess)
    (/ (+ guess (/ x guess)) 2)
    )
  (define (sqrt-iter guess)
    (if (good? guess) guess
        (sqrt-iter (improve guess))
        )
    )
  (sqrt-iter 1.0)

  )
(sqrt 10)

  