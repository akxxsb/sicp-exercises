(load "include/common.scm")
(load "include/format.scm")
(load "include/accumulate-utils.scm")
(load "include/math.scm")

;包含习题1.29至1.41

(println "====================== 1.29 =====================")

(define (integral f a b dx)
  (define (add-dx x) (+ x dx))
  (* (sum f (+ a (/ dx 2.0)) add-dx b) dx))

(define (simpson-integral f a b n)
  (let ((h (/ (- b a) n))); 区间间隔h = (b-a)/n
    (define (term k)      ; 定义term函数
      (define x (+ a (* k h)))
      (define yk (f x))
      (cond ((or (= k 0) (= k n)) (* 1.0 yk))
        ((even? k) (* 2.0 yk))
        (else (* 4.0 yk))))
    (* (/ h 3.0) (sum term 0 inc n))
  )
)

(println "integral for cube func at [0, 1]")
(display "dx=0.01, ans:") (println (integral cube 0 1 0.01))
(display "dx=0.001, ans:") (println (integral cube 0 1 0.001))

(println "simpson-integral for cube func at [0, 1]")
(display "n=100, ans:") (println (simpson-integral cube 0 1 100))
(display "n=1000, ans:") (println (simpson-integral cube 0 1 1000))

(println "====================== 1.30 =====================")
(define (sum-iter term a next b)
  (define (iter a result)
    (if (> a b)
      result
      (iter (next a) (+ result (term a)))))
  (iter a 0)
)

(define (integral-iter f a b dx)
  (define (add-dx x) (+ x dx))
  (* (sum-iter f (+ a (/ dx 2.0)) add-dx b) dx))

(println "integral-iter for cube func at [0, 1]")
(display "dx=0.01, ans:") (println (integral-iter cube 0 1 0.01))
(display "dx=0.001, ans:") (println (integral-iter cube 0 1 0.001))

(println "====================== 1.31 =====================")

(define (get-pi n)
  (define (term k)
    (cond ((odd? k) (/ (+ k 1) (+ k 2)))
      (else (/ (+ k 2) (+ k 1)))))
  (* 4 (product-iter term 1 inc n)))

(display "factorial(5) = ") (println (factorial 5))
(display "factorial-iter(12) = ") (println (factorial-iter 12))
(display "get-pi 10000 = ") (println (get-pi 10000))

(println "====================== 1.32 =====================")
(define (accumulate combiner null-value term a next b)
  (if (> a b)
    null-value
    (combiner (term a)
      (accumulate combiner null-value term (next a) next b)))
)

(define (sum-by-accumulate term a next b)
  (accumulate + 0 term a next b))

(define (product-by-accumulate-iter term a next b)
  (accumulate-iter * 1 term a next b))

(display "(sum-by-accumulate self 1 inc 100): ") (println (sum-by-accumulate self 1 inc 100))
(display "(product-by-accumulate-iter self 1 inc 10): ") (println (product-by-accumulate-iter self 1 inc 10))

(println "====================== 1.33 =====================")

(define (get-prime-sum a b)
  (filtered-accumulate prime? + 0 self a inc b))

; 计算满足gcd(n,x) = 1 的所有x的乘积
(define (get-gcd-is-1-product n)
  (define (filter? x)
    (= (gcd x n) 1))
  (filtered-accumulate filter? * 1 self 1 inc n)
)

(display "(get-prime-sum 12 50): ") (println (get-prime-sum 12 50))
(display "(get-gcd-is-1-product 10): ") (println (get-gcd-is-1-product 10))

(println "====================== 1.34 =====================")
(define (f g)
  (g 2))

(println (f square))

(println "====================== 1.35 =====================")

(println (fixed-point (lambda (x) (+ 1 (/ 1 x))) 1.0))

(println "====================== 1.36 =====================")
;寻找不动点并打印过程中的近似值
(define (fixed-point-v2 f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (println next) ; 打印每次迭代的近似值
      (if (close-enough? guess next)
        next
        (try next))))
  (try first-guess))

(println "not damped:")
(println (fixed-point-v2 (lambda (x) (/ (log 1000) (log x)) ) 2.0))
(println "damped:")
(println (fixed-point-v2 (average-damp (lambda (x) (/ (log 1000) (log x)) )) 2.0))

(println "====================== 1.37 =====================")
(println (cont-frac-iter one-float one-float 100))

(println "====================== 1.38 =====================")
(define e-2 (cont-frac-iter
              one-float
              (lambda (i)
                (cond ((= (remainder i 3) 2) (* 2 (/ (+ i 1) 3)))
                  (else 1)
                )
              )
              10000))

(printbln "e-2: " e-2 "e:" (sum (lambda (i) (/ 1.0 (factorial i))) 0 inc 1000))

(println "====================== 1.39 =====================")
(define (tan-cf x k)
  (cont-frac-iter (lambda (i) ;n(i)定义
    (if (= i 1) x (- (square x))))
    (lambda (i) (- (* 2 i) 1)) ;d(i)定义
    1000))
(printbln "tan(pi/4): " (tan-cf (/ (get-pi 1000) 4) 1000))

(println "====================== 1.40 =====================")

; 函数x^3 + ax^2 + bx + c
(define (cubeic a b c)
  (lambda (x) (+ 
    (cube x)
    (* a (square x))
    (* b x)
    c)))

(define g1 (cubeic 3 2 1))
(define g2 (cubeic 4 (- 3) 2))
(define g1x (newtons-method g1 1))
(define g2x (newtons-method g2 1))
(println "g1: x^3 + 3x^2 + 2x + 1 = 0 root: " g1x ", g1(x) = " (g1 g1x))
(println "g2: x^3 + 4x^2 -3x + 2 = 0 root: " g2x ", g2(x) = " (g2 g2x))

(println "====================== 1.41 =====================")

(define (double f)
  (lambda (x) (f (f x))))

(define (inc x)
  (+ x 1))

(println ((double inc) 0))
(println (((double double) inc) 0))
(println (((double (double double)) inc) 0))
(println (((double (double (double double))) inc) 0))