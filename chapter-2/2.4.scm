(load "include/format.scm")

(println "====================== 2.4 =====================")
(define (cons-v1 x y)
  (lambda (m) (m x y)))

(define (car-v1 z)
  (z (lambda (x y) x)))

(define (cdr-v1 z)
  (z (lambda (x y) y)))


(let ((c1 (cons-v1 2 3)))
  (printbln "x:" (car-v1 c1) "y:" (cdr-v1 c1)))