(define (start-segment x y)
  (make-point x y))

(define (end-segment x y)
  (make-point x y))


(define (x-point p)
  (car p))

(define (y-point p)
  (cadr p))

(define (make-point x y)
  (list x y))

(define (print-point p)
  (newline)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")"))


(define (make-segment x1 y1 x2 y2)
  (list (start-segment x1 y1) (end-segment x2 y2)))


(define (midpoint seg)
    (list (/ (+ (caar seg) (caadr seg)) 2) (/ (+ (cadadr seg) (cadar seg)) 2)))