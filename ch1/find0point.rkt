#lang racket

(define (println str)
  (display str)
  (newline)
  )
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
  (println first-guess)
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

;1.35
;1.36找不动点 x的x次方 = 1000
(fix-point (lambda (x) (/ (log 1000) (log x))) 2.0)



;1.37
(define (cont-frac n d k)
  (define (cont-iter total count)
    (if (= count 0) total
        (cont-iter (/ (n count) (+ (d count) total)) (- count 1))
        )
    )
  (cont-iter 0 k)
  )


(cont-frac (lambda (i) 1.0) (lambda (i) 1.0) 10)

; 1.37验证黄金分割
(define a (cont-frac (lambda (i) 1.0) (lambda (i) 1.0) 10))


; 1.37 2 采用另一种计算过程
(define (cont-frac2 n d k)
  (define (cont-recu count)
    (if (= count k) (/ (n count) (d count))
        (/ (n count) (+ (d count) (cont-recu (+ count 1))))))
  (cont-recu 0)
  )
(cont-frac2 (lambda (i) 1.0) (lambda (i) 1.0) 10)

; 1.38

(define (divide3? n) (= (remainder n 3) 0))

(define (oula-d i)
  (cond ((divide3? (+ i 1)) (* 2 (/ (+ i 1) 3)))
        ((= i 1) 1)
        (else 1)
  )
  )

; 欧拉Continued Fraction求e
(cont-frac (lambda (i) 1.0) oula-d 1000)

; 1.39
(define (power base n)
  (if (= n 0) 1 (* base (power base (- n 1))))
  )
(define (tan-cf x k)
  (cont-frac (lambda (i) (if (= i 1) x (- (square x))))
             (lambda (i) (- (* 2 i) 1))
             k)
  )
(tan-cf 2.0 100)
(tan 2)

(define (average-dump f)
  (lambda (x) (average x (f x))))


(fix-point (average-dump (lambda (x) (/ 2 x))) 1.0)
(define (cube-root x)
  (fix-point (average-dump (lambda (y) (/ x (square y)))) 1.0))

(cube-root 27)


(define dx 0.00001)
(define (deriv g)
  (lambda (x) (/ (- (g (+ x dx)) (g x)) dx)))

(define (cube x) (* x x x))
((deriv cube) 5)

(define (newton-transform g)
  (lambda (x) (- x (/ (g x) ((deriv g) x)))))
  

(define (newtons-method g guess)
  (fix-point (newton-transform g) guess))