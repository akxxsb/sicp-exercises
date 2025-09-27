(define (filtered-accumulate filter combiner null-value term a next b)
  (define (iter a result)
    (cond ((> a b) result)
      ((filter a) (iter (next a) (combiner result (term a))))
      (else (iter (next a) result))))
  (iter a null-value))

(define (accumulate-iter combiner null-value term a next b)
  (filtered-accumulate true combiner null-value term a next b))

(define (sum term a next b)
  (accumulate-iter + 0 term a next b))

(define (product term a next b)
  (accumulate-iter * 1 term a next b))

(define (product-iter term a next b)
  (define (iter a result)
    (if (> a b)
      result
      (iter (next a) (* result (term a)))))
  (iter a 1.0))