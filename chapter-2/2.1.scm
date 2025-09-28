(load "include/math.scm")
(load "include/format.scm")

(define make-rat-bad
  (lambda (n d)
    (let ((g (gcd n d)))
      (cons (/ n g) (/ d g)))))

(println "====================== 2.1 =====================")
(define (make-rat n d)
  (if (< d 0)
    (make-rat-bad (- n) (- d))
    (make-rat-bad n d)))

(define (numer x) (car x))
(define (denom x) (cdr x))

(define (add-rat x y)
  (make-rat
    (+ (* (numer x) (denom y))
      (* (numer y) (denom x)))
    (* (denom x) (denom y))))

(define (sub-rat x y)
  (make-rat
    (- (* (numer x) (denom y))
      (* (numer y) (denom x)))
    (* (denom x) (denom y))))

(define (mul-rat x y)
  (make-rat
    (* (numer x) (numer y))
    (* (denom x) (denom y))))

(define (div-rat x y)
  (make-rat
    (* (numer x) (denom y))
    (* (denom x) (numer y))))

(define eq-rat?
  (lambda (x y)
    (and (= (* (numer x) (denom y))
           (* (numer y) (denom x)))
      (= (denom x) (denom y)))))

(define (print-rat x)
  (display (numer x))
  (display "/")
  (display (denom x))
  (newline))

(define one-half (make-rat 1 2))
(define one-third (make-rat 1 3))
(print-rat one-half)
(print-rat (add-rat one-half one-third))
(print-rat (sub-rat one-half one-third))
(print-rat (mul-rat one-half one-third))
(print-rat (div-rat one-half one-third))
(display (eq-rat? one-half (make-rat 2 4)))
