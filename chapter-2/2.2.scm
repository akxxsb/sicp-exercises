(load "include/format.scm")
; point定义
(define (make-point x y) (cons x y))
(define (x-point p) (car p))
(define (y-point p) (cdr p))

; segment定义
(define (make-segment p1 p2) (cons p1 p2))
(define (start-segment seg) (car seg))
(define (end-segment seg) (cdr seg))

(define (print-point p) (println "(" (x-point p) "," (y-point p) ")"))

(println "====================== 2.2 =====================")
(define (midpoint-segment seg) 
  (make-point (/ (+ (x-point (start-segment seg)) (x-point (end-segment seg))) 2)
              (/ (+ (y-point (start-segment seg)) (y-point (end-segment seg))) 2)))

(define s1 (make-segment (make-point 1 2) (make-point 3 7)))
(printbln "seg (1, 2) to (3, 7) 's mid-point = " (midpoint-segment s1))