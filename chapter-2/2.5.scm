(load "include/format.scm")
(load "include/math.scm")

(println "====================== 2.5 =====================")
; 将序对(x, y)表示为2^x*3^y对应整数
(define (cons-v2 x y)
  (* (fast-expt 2 x) (fast-expt 3 y)))

(define (get-divisor-cnt n b)
  (define (iter n cnt)
    (if (= (remainder n b) 0)
      (iter (/ n b) (+ cnt 1))
      cnt))
  (iter n 0))

(define (car-v2 n)
  (get-divisor-cnt n 2))

(define (cdr-v2 n)
  (get-divisor-cnt n 3))

(let ((c1 (cons-v2 2 3)))
  (printbln "c1:" c1 "x:" (car-v2 c1) "y:" (cdr-v2 c1)))