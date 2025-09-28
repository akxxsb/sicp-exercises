(load "chapter-2/2.2.scm") ; point定义
(load "include/format.scm")

(println "====================== 2.3 =====================")


(define (make-rect-v1 left-bottom-point w h) (cons left-bottom-point (cons w h)))
(define (make-rect-v2 left-bottom-point right-top-point)
  (cons left-bottom-point (cons (- (x-point right-top-point) (x-point left-bottom-point))
                               (- (y-point right-top-point) (y-point left-bottom-point)))))

(define (rect-width rect) (car (cdr rect)))
(define (rect-height rect) (cdr (cdr rect)))

(define (rect-perimeter rect) (* 2 (+ (rect-width rect) (rect-height rect))))
(define (rect-area rect) (* (rect-width rect) (rect-height rect)))

(let ((rect1 (make-rect-v1 (make-point 0 0) 3 2))
      (rect2 (make-rect-v2 (make-point 0 0) (make-point 2 2))))
    (printbln "retc1 perimeter = " (rect-perimeter rect1) "retc1 area = " (rect-area rect1))
    (printbln "retc2 perimeter = " (rect-perimeter rect2) "retc2 area = " (rect-area rect2)))

