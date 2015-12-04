#lang racket
;; 空环境
(define env0 '())

;; 扩展环境
(define ext-env
  (lambda (x v env)
    (cons `(,x . ,v) env)))

;; 查找。在环境 env 中查找 x 的值
(define lookup
  (lambda (x env)
    (let ([p (assq x env)])
      (cond
        [(not p) x]
        [else (cdr p)]))))

;; 闭包的数据结构定义，包含一个函数定义f和它定义时所在的环境
(struct Closure (f env))



(define interpl
  (lambda (exp env)
    (match exp
      [(? symbol? x) (lookup x env)]
      [(? number? x) x]
      [`(lambda (,x) , e)
       (Closure exp env)]
      [`(,e1, e2)
       (let ([v1 (interpl e1 env)]
             [v2 (interpl e2 env)])
         (match v1
           [(Closure `(lambda (,x) ,e) env1)
            (interpl e (ext-env x v2 env1))]))]
      [`(,op, e1, e2)
       (let ([v1 (interpl e1 env)]
             [v2 (interpl e2 env)])
         (match op
           ['+ (+ v1 v2)]
           ['- (- v1 v2)]
           ['* (* v1 v2)]
           ['/ (/ v1 v2)]))])))

(define interp
  (lambda (exp)
    (interpl exp env0)))



(((lambda (y) ((lambda (y) (lambda (x) (* x y))) 2)) 3) 4)
         