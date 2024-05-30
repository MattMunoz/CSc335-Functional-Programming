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


(define (rectangle x1 y1 x2 y2 x3 y3 x4 y4)
  (list (make-segment x1 y1 x2 y2)
        (make-segment x2 y2 x3 y3)
        (make-segment x3 y3 x4 y4)
        (make-segment x4 y4 x1 y1)))

(define (segment-length seg)
  (sqrt (+ (expt (- (caadr seg) (caar seg)) 2) (expt (- (cadadr seg) (cadar seg)) 2))))

(define (perimeter rec)
  (+ (segment-length (car rec))
     (segment-length (cadr rec))
     (segment-length (caddr rec))
     (segment-length (cadddr rec))))

(define (area rec)
  (* (leng rec) (height rec)))

(define (height rec)
  (segment-length (car rec)))

(define (leng rec)
  (segment-length (cadr rec)))

(define (print-rectangle r)
  (display (car r))
  (newline)
  (display (cadr r))
  (newline)
  (display (caddr r))
  (newline)
  (display (cadddr r)))
