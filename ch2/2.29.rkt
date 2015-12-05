#lang racket
(define (make-mobile left right)
  (list left right))
(define (mobile-left mobile)
  (car mobile))
(define (mobile-right mobile)
  (cadr mobile))

(define (make-branch length structure)
  (list length structure))
(define (branch-length branch)
  (car branch))
(define (branch-structure branch)
  (cadr branch))

(define (total-weight mobile)
  (define (total-w-branch branch)
    (if (not (pair? (branch-structure branch))) (branch-structure branch)
        (total-weight (branch-structure branch))))
  (+ (total-w-branch (mobile-left mobile)) (total-w-branch (mobile-right mobile))))

;; 力矩
(define (moment branch)
  (if (not (pair? (branch-structure branch))) (* (branch-length branch) (branch-structure branch))
      (* (branch-length branch) (total-weight (branch-structure branch)))))

(define (balance? mobile)
  (= (moment (mobile-left mobile)) (moment (mobile-right mobile))))


(define left-b (make-branch 10 3))
(define right-b (make-branch 2 4))
(define mobile (make-mobile left-b right-b))
(total-weight mobile)


(define com-mobile (make-mobile (make-branch 3 mobile) (make-branch 10 242)))
(total-weight com-mobile)

(balance? com-mobile)

