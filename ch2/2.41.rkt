#lang racket
(require "../commons.rkt")


(define (make-tribles n)
  (flatmap
   (lambda (i)
     (flatmap
      (lambda (j)
        (map
          (lambda (k) (list i j k))
         (enumerate-interval 1 (- j 1))
        ))
      (enumerate-interval 1 (- i 1)))
     )
  (enumerate-interval 1 n))
  )

(make-tribles 10)


