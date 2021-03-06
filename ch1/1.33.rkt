#lang racket


(define (square n)
  (* n n)
  )
(define (next-divider n)
  (if (= n 2) 3
      (+ n 2)
      )
  )
(define (min-divider n)
  (define (divide-iter i)
    (cond ((> (square i) n) n)
      ((= (remainder n i) 0) i)
      (else (divide-iter (next-divider i)))
      )
    )
  (divide-iter 2)
  )
      
(define (prime? n)
  (= (min-divider n) n)
  )
(define (filter-accumulate filter accumulate term null-value a next b)
  (define (filter-iter total a)
  (cond ((> a b) total)
        ((not (filter (term a))) (filter-iter total (next a)))
       (else (filter-iter (accumulate total (term a)) (next a)))
       )
    )
  (filter-iter null-value a)
  )
(define (ide x) x)
(define (inc x) (+ x 1))
(filter-accumulate prime? + ide 0 2 inc 10000)
(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))
      )
  )
(define (primemulti n)
  (filter-accumulate (lambda (x) (= (gcd n x) 1)) * ide 1 1 inc n)
  )
(primemulti 7)