;约定库私有函数以-开头，不对外暴露
(define (-print-impl sep args)
  (if (not (null? args))
    (begin
      (display (car args))
      (display sep)
      (-print-impl sep (cdr args))
    )))

(define (print . args)
  (-print-impl "" args))

(define (println . args)
  (-print-impl "" args)
  (newline))

(define (printb . args)
  (-print-impl " " args))

(define (printbln . args)
  (-print-impl " " args)
  (newline))

(define (output-blank n)
  (if (> n 0)
    (begin
      (display " ")
      (output-blank (- n 1))
    )
  )
)