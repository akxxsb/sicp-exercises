(load "include/common.scm")
(load "include/accumulate-utils.scm")

(define (square x)
  (* x x))

(define (cube x)
  (* x x x))

(define (even? n)
  (= (remainder n 2) 0))

(define (odd? n)
  (not (even? n)))

;判断a能否整除b
(define (divides? a b)
  (= (remainder b a) 0))

;查找n 从test-divisor开始的第一个因子
(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
    ((divides? test-divisor n) test-divisor)
    (else (find-divisor n (+ test-divisor 1)))))

(define (prime? n)
  (= (find-divisor n 2) n))

(define (gcd a b)
  (if (or (= a 0) (= b 0))
    (+ a b)
    (gcd b (remainder a b))))

(define one-float (lambda (x) 1.0))

(define (factorial n)
  (product self 1 inc n)
)

(define (factorial-iter n)
  (product-iter self 1 inc n)
)

(define tolerance 0.00001)
(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess iter-count)
    (let ((next (f guess)))
      ;(println "guess[" iter-count "]: " next)
      (if (close-enough? guess next)
        next
        (try next (+ iter-count 1)))))
  (try first-guess 1))

(define (average-damp f)
  (lambda (x) (/ (+ x (f x)) 2)))

;求函数的导数
(define (deriv g)
  (define dx 0.000001)
  (lambda (x) (/ (- (g (+ x dx)) (g x)) dx)))

; 牛顿变换,用于求解函数g(x) = 0的根
; 令f(x) = x - g(x)/g'(x)
; 则g(x) = 0的根x*, 满足 f(x*) = x*, 和f(x)的不动点等价
(define (newton-transfrom g)
  (lambda (x) (- x (/ (g x) ((deriv g) x)))))

; 牛顿法求g(x) = 0的根
(define (newtons-method g guess)
  (fixed-point (newton-transfrom g) guess))

; 计算连分数
(define (cont-frac n d k)
  (define (cont-frac-inner i)
    (if (= i k)
      (/ (n i) (d i))
      (/ (n i) (+ (d i) (cont-frac-inner (+ i 1))))))
  (cont-frac-inner 1))

; 计算连分数迭代版
(define (cont-frac-iter n d k)
  (define (cont-frac-inner i result)
    (cond ((= i 0) result)
      (else (cont-frac-inner (- i 1) (/ (n i) (+ (d i) result))))))
  (cont-frac-inner (- k 1) (/ (n k) (d k))))

(define (fast-expt b n)
  (cond ((= n 0) 1)
    ((even? n) (square (fast-expt b (/ n 2))) )
    (else (* b (fast-expt b (- n 1)))))
)