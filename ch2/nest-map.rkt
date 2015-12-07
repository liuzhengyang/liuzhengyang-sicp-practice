#lang racket
(require "../commons.rkt")

(define (enumerate-interval low high)
  (if (> low high) nil
      (cons low
            (enumerate-interval (+ low 1) high))))

(enumerate-interval 1 10)

(define (flat-map proc seq)
  (accumulate append nil (map proc seq)))


(define (prime-sum? pair)
  (prime? (+ (car pair) (cadr pair))))

(define (make-pair-sum pair)
  (let ((a (car pair))
        (b (cadr pair)))
  (list a b (+ a b))))

(prime-sum? (list 4 2))
(make-pair-sum (list 1 9))

(define (unique-pairs n)
  (flatmap
   (lambda (i) (map (lambda (j) (list i j)) (enumerate-interval 1 (- i 1)))) 
  (enumerate-interval 1 n)))

(define (prime-sum-pairs2 n)
  (map make-pair-sum
       (filter prime-sum?
               (unique-pairs n))))

(define (prime-sum-pairs n)
  (map make-pair-sum
     (filter
 prime-sum?
        (flatmap
 (lambda (i) (
              map
              (lambda (j) (list i j))
              (enumerate-interval 1 (- i 1))
              ))
 (enumerate-interval 1 n)))))

(define (permutations s)
  (if (null? s) (list nil)
      (flatmap
         (lambda (item)
           (map
            (lambda (p) (cons item p))
                           (permutations (remove item s))))
       s)))

(define (remove item seq)
  (filter (lambda (x) (not (= item x))) seq))