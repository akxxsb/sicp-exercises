(define (false . args)#f)
(define (true . args)#t)

(define (self n)
  n)

(define (nop . args)
  (self args))

(define (inc n)
  (+ n 1))

(define (length items)
  (if (null? items)
    0
    (+ 1 (length (cdr items)))))