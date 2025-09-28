(load "include/common.scm")
(load "include/format.scm")

(println "====================== 2.6 =====================")

; 邱齐数n定义
; (n)返回一个匿名函数t，t接受一个参数f, 对t的调用 (t f)返回一个匿名函数g
; 匿名函数g接受输入x, 对g的调用(g x)的效果为对x应用n次f f(f(...f(x)));
(define (zero) (lambda (f) (lambda (x) x)))

; 基于n定义n+1
; 输入n,n为邱齐数
; 返回邱齐数n+1
(define (add-1 n)
  (lambda () (lambda (f) (lambda (x) (f (((n) f) x))))))

; 定义add-k, 基于n和k定义邱齐数n+k
(define (add-k n k)
  (define (add-k-iter n k f x)
    (if (> k 0)
      (add-k-iter n (- k 1) f (f x))
      (((n) f) x)))
  (lambda () (lambda (f) (lambda (x) (add-k-iter n k f x)))))

; 通过repeated高阶函数定义add-k-v2
(define (add-k-v2 n k)
  ((repeated add-1 k) n))

; 直接定义邱齐数one 和 two
(define (one) (lambda (f) (lambda (x) (f x))))
(define (two) (lambda (f) (lambda (x) (f (f x)))))

; 对two应用一次add-1 定义three
(define three (add-1 two))
; 对two应用2次add-1 定义four
(define four (add-1 (add-1 two)))
; 基于one 和 add-k定义five
(define five (add-k one 4))
; 基于three 和 add-k-v2 定义six
(define six (add-k-v2 three 3))

(println "(((one) inc) 0) = " (((one) inc) 0))
(println "(((two) inc) 0) = " (((two) inc) 0))
(println "(((three) inc) 0) = " (((three) inc) 0))
(println "(((four) inc) 0) = " (((four) inc) 0))
(println "(((five) inc) 0) = " (((five) inc) 0))
(println "(((six) inc) 0) = " (((six) inc) 0))
