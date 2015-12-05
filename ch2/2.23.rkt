#lang racket


(define (for-each proc items)
  (define (for-iter item)
    (proc item)
    (for-each proc (cdr items)))
  (unless (null? items)
      (for-iter (car items)))
  )

(for-each (lambda (x) (newline) (display x)) (list 1 2 34 5 5 52 3523 ))



(cons 2 (list 1))

(cons '2 '())
(cons 2 '())

(define (count-leaves x)
  (cond ((null? x) 0)
        ((not (pair? x)) 1)
        (else (+ (count-leaves (car x)) (count-leaves (cdr x))))))

(define x (cons (list 1 2) (list 3 4)))
(count-leaves x)

(list 1 (list 2 (list 3 (list 4 (list 5 6)))))

x


(define l '(1 3 (5 7) 9))
l
(define (get7 l)
 (car (cdr (car (cdr (cdr l))))))

(define l2 (list (list 7)))
(car (car l2))

(define l3 '(1 (2 (3 (4 (5 (6 7)))))))

(define l1 (list 1 (list 2 (list 3 (list 4 5)))))
(car l1)
(cdr l1)
(car (cdr l1))
(car l1)
(cdr l3)
(cdr (cdr l3))
(cdr '(6 7))

(cdr (cdr l3))
;(define (get7-2 l)
  ;((car (cdr (cdr (cdr (cdr (cdr  (cdr l3)))))))))
;(get7-2 l3)
(get7 l)