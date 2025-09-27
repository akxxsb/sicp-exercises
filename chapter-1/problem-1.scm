
(load "include/common.scm")
(load "include/format.scm")
(load "include/math.scm")

(println "====================== 1.3 =====================")
;定义sum-of-two-largest函数，计算三个数中较大的两个数的和
(define (sum-of-two-largest a b c)
  (cond ((and (>= a c) (>= b c)) (+ a b))
        ((and (>= a b) (>= c b)) (+ a c))
        (else (+ b c))))

; 调用函数并打印结果
(println (sum-of-two-largest 4 5 3))
(println (sum-of-two-largest 1 2 3))
(println (sum-of-two-largest 10 5 7))

(println "====================== 1.4 =====================")
(define (a-plus-abs-b a b)
  ((if (> b 0) + -) a b))

(println (a-plus-abs-b 1 2))   ; 输出 3
(println (a-plus-abs-b 1 -2))  ; 输出 3

(println "====================== 1.5 =====================")
(define (p) (p)) ; 无限递归调用
(define (test x y)
  (if (= x 0)
      0
      y))
;(println (test 0 (p)))  ; 输出 0
; 由于 Scheme 使用 applicative-order 求值策略，(p) 会被求值
; 导致无限递归调用，从而程序无法正常结束
; 如果使用 normal-order 求值策略，(p) 不会被求值，程序会正常输出 0
; 这说明求值策略会影响程序的行为
(println "====================== 1.6 =====================")

(define (good-enough? guess x)
  (< (abs (- (square guess) x)) 0.001))

(define (improve guess x)
  (/ (+ guess (/ x guess)) 2))

(define (sqrt x)
  (define (sqrt-iter guess x)
    (if (good-enough? guess x)
        guess
        (sqrt-iter (improve guess x) x)))
  (sqrt-iter 1.0 x))

(println (sqrt 2))

(define (new-if condition then else)
  (cond (condition then)
        (else else)))

(define (sqrt-new-if x)
  (define (sqrt-iter guess x)
    (new-if (good-enough? guess x)
        guess
        (sqrt-iter (improve guess x) x)))
  (sqrt-iter 1.0 x))

;对 sqrt-new-if 的调用会导致栈溢出,因为 new-if 会立即求值 then 和 else 分支
;从而导致无限递归调用 sqrt-iter
;(println (sqrt-new-if 2))

(println "====================== 1.7 =====================")

(define (good-enough-by-change-rate? prev-guess guess)
  (< (abs (/ (- guess prev-guess) guess)) 0.001))

(define (sqrt-by-change-rate x)
  (define (sqrt-iter prev-guess guess x)
    (if (good-enough-by-change-rate? prev-guess guess)
        guess
        (sqrt-iter guess (improve guess x) x)))
  (sqrt-iter 0.0 1.0 x))

(println (sqrt-by-change-rate 2))
(println (sqrt-by-change-rate 0.0004))
(println (sqrt-by-change-rate 100000000))

(println "====================== 1.8 =====================")

(define (cube x)
  (define (improve guess x)
  (/ (+ (/ x (square guess)) (* 2 guess)) 3))

  (define (cube-iter prev-guess guess x)
    (if (good-enough-by-change-rate? prev-guess guess)
      guess
      (cube-iter guess (improve guess x) x)))
  (cube-iter 0.0 1.0 x))

  (println (cube 8))
  (println (cube 27))
  (println (cube 12))

(println "====================== 1.11 =====================")

; 采用递归方式计算f(n) = f(n-1) + 2f(n-2) + 3f(n-3)
(define (func1.11-v1 n)
  (if (< n 3)
    n
    (+ (func1.11-v1 (- n 1))
      (* 2 (func1.11-v1 (- n 2)))
      (* 3 (func1.11-v1 (- n 3))))))
(println (func1.11-v1 4))

; 采用迭代方式计算f(n) = f(n-1) + 2f(n-2) + 3f(n-3)
(define (func1.11-v2 n)
  (define (f-iter a b c idx)
    (cond ((= idx 0) c)
      ((= idx 1) b)
      ((= idx 2) a)
      (else (f-iter (+ a (* 2 b) (* 3 c)) a b (- idx 1)))))
  (f-iter 2 1 0 n))
(println (func1.11-v2 4))

(println "====================== 1.12 =====================")

; 输出杨辉三角
(define (output-trangle n)
  (define (get-data row col)
    (if (or (= col 1) (= col row))
      1
      (+ (get-data (- row 1) (- col 1)) (get-data (- row 1) col)) 
    )
  )
  (define (output-blank-and-newline row col)
    (if (<= row n)
      (if (= col 1)
        (begin
          ;非第一行起始,先输出换行
          (if (> row 1)
            (newline)
            #f
          )
          ;输出前置空格
          (output-blank (- n row))
        )
        ;非第一列, 输出一个空格和后面的数字分割开
        (output-blank 1)
      )
      #f
    )
  )
  (define (output-trangle-inner row col)
    (if (and (<= row n) (<= col row))
      (begin
        (output-blank-and-newline row col)
        (display (get-data row col))
        (if (< col row)
          (output-trangle-inner row (+ col 1)); 非最后一列，继续输出下一列
          (output-trangle-inner (+ row 1) 1);   最后一列，继续输出下一行
        )
      )
      #f
    )
  )
  (output-trangle-inner 1 1)
  (newline))

(output-trangle 5)

(println "====================== 1.16 =====================")

(define (fast-expt b n)
  (cond ((= n 0) 1)
    ((even? n) (square (fast-expt b (/ n 2))) )
    (else (* b (fast-expt b (- n 1)))))
)
(println (fast-expt 3 5))

(define (fast-expt-iter b n a)
  (cond ((= n 0) a)
    ((even? n) (fast-expt-iter (square b) (/ n 2) a))
    (else (fast-expt-iter b (- n 1) (* a b)))))
(println (fast-expt-iter 3 5 1))
(println (fast-expt-iter 3 6 1))

(println "====================== 1.17 =====================")

(define (double x)
  (+ x x))
(define (halve x)
  (/ x 2))

(define (multi a b)
  (cond ((= b 0) 0)
    ((even? b) (multi (double a) (halve b)))
    (else (+ a (multi a (- b 1))))))

(println (multi 5 7))

(println "====================== 1.18 =====================")
(define (multi-iter a b s)
  (cond ((= b 0) s)
    ((even? b) (multi-iter (double a) (halve b) s))
    (else (multi-iter a (- b 1) (+ s a)))))

(println (multi-iter 5 7 0))

(println "====================== 1.19 =====================")

(define (fib n)
  (fib-iter 1 0 0 1 n))

; 快速裴波拉切数列
; 令T(p,q) 变换 a' = (p+q)a + qb, b' = qa + pb
; 则引用两次变换后 b'' = (q(p+q)+pq)a + (q^2+p^2)b
; 则p' = q^2 + p^2, q' = (q(p+q)+pq)
(define (fib-iter a b p q n)
  (define (next-p p q)
    (+ (square q) (square p)))
  (define (next-q p q)
    (+ (* q (+ p q)) (* p q)))

  (cond ((= n 0) b)
    ((even? n) (fib-iter a b (next-p p q) (next-q p q) (/ n 2)))
    (else (fib-iter (+ (* q b) (* (+ p q) a))
                    (+ (* q a) (* p b))
                    p
                    q
                    (- n 1)))))

; 输出前n项fib
(define (output-fib-n n)
  (if (> n 0)
    (begin 
      (output-fib-n (- n 1))
      (printb (fib n))
    ))
  (newline))
(output-fib-n 10)

(println "====================== 1.21 =====================")
(define (smallest-divisor n)
  (find-divisor n 2))

(println (smallest-divisor 199))
(println (smallest-divisor 1999))
(println (smallest-divisor 19999))

(println "====================== 1.22 =====================")

(define (timed-prime-test n)
  (newline)
  (display n)
  (start-prime-test n (real-time-clock)))

(define (start-prime-test n start-time)
  (if (prime? n)
    (report-prime (- (real-time-clock) start-time))))

(define (report-prime elapsed-time)
  (display " *** ")
  (display elapsed-time))

(define (next-odd n)
  (if (odd? n)
    (+ n 2)
    (+ n 1)))

; 计算从[n+1,+>>)的count个的素数
(define (next-k-prime n count)
  (define (helper x k)
    (if (> k 0)
      (if (prime? x)
        (begin
          (display x)
          (println " *** ")
          (helper (next-odd x) (- k 1)))
        (helper (next-odd x) k))))
  (helper (+ n 1) count))

(define (search-for-primes a count)
  (define start-time (real-time-clock))
  (next-k-prime a count)
  (display "search for prime used time:")
  (println (- (real-time-clock) start-time))
)

(search-for-primes 1000 3)
(search-for-primes 10000 3)
(search-for-primes 100000 3)
(search-for-primes 1000000 3)