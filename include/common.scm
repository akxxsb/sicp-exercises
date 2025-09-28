(define (false . args)#f)
(define (true . args)#t)

(define (self n)
  n)

(define (nop . args)
  (self args))

(define (inc n)
  (+ n 1))

(define (compose f g)
  (lambda (x) (f (g x))))

(define (repeated f n)
  (cond ((< n 1) nop)
    ((= n 1) f)
    (else (compose f (repeated f (- n 1))))))

(define (length items)
  (if (null? items)
    0
    (+ 1 (length (cdr items)))))