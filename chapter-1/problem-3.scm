(load "include/format.scm")
(load "include/common.scm")
(load "include/math.scm")

;包含习题1.42至1.46
(println "====================== 1.42 =====================")
(define (compose f g)
  (lambda (x) (f (g x))))

(printbln "((compose square inc) 6) = " ((compose square inc) 6))

(println "====================== 1.43 =====================")
(define (repeated f n)
  (cond ((< n 1) nop)
    ((= n 1) f)
    (else (compose f (repeated f (- n 1))))))

(println "((repeated inc 10) 0) = " ((repeated inc 10) 0))
(println "((repeated square 3) 2) = " ((repeated square 3) 2))
(println "((repeated square 0) 2) = " ((repeated square 0) 2))

(println "====================== 1.44 =====================")
(define (smooth f)
  (let ((dx 0.000001))
    (lambda (x) (/ (+ (f (- x dx)) (f x) (f (+ x dx))) 3))))

(define (smooth-n f n)
  ((repeated smooth n) f))

(println "((smooth square) 2) = " ((smooth square) 2))
(println "((smooth-n square 5) 2) = " ((smooth-n square 5) 2))

(println "====================== 1.45 =====================")
; 计算y的n次方根，满足x^n = y
; 两边同除x^(n-1), 即x = y/(x^(n-1))
; 等价于求x = y/(x^(n-1))的不动点 x*
(define (get-n-th-root y n)
  (define g (lambda (x) (/ y (fast-expt x (- n 1)))))
  (define transfrom-func ((repeated average-damp (- n 1)) g))
  (fixed-point transfrom-func 1.0))

(printbln "(get-n-th-root 15 4) = " (get-n-th-root 15 4))
(printbln "(get-n-th-root 1024 2) = " (get-n-th-root 1024 2))

(println "====================== 1.46 =====================")
(define (iterative-improve good-enough? improve-guess)
  (define (iter guess)
    (if (good-enough? guess)
      guess
      (iter (improve-guess guess))))
  iter)

(define (sqrt-v3 n)
  (define (good-enough? guess)
    (< (abs (- (square guess) n)) 0.001))

  (define (improve guess)
    (/ (+ guess (/ n guess)) 2))

  ((iterative-improve
    good-enough?
    improve) 1.0))

; 基于iterative-improve重新定义不动点求解
(define (fixed-point-v3 f first-guess)
  (define (good-enough? guess)
    (< (abs (- guess (f guess))) tolerance))
  (define (improve guess)
    (f guess))
  ((iterative-improve
    good-enough?
    improve) first-guess))

(printbln "(sqrt-v3 16) = " (sqrt-v3 16))
(println "fixed-point of 1+1/x = " (fixed-point-v3 (lambda (x) (+ 1 (/ 1 x))) 1.0))
