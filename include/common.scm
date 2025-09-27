(define (false . args)#f)
(define (true . args)#t)
(define (nop . args)())

(define (choose-if p tv fv)
  (if p
    tv
    fv))

(define (inc n)
  (+ n 1))

(define (self n)
  n)

(define (length items)
  (if (null? items)
    0
    (+ 1 (length (cdr items)))))