#|

(define cube
  (lambda (x) (* x x x)))

(define (inc n) (+ n 1))
(define (sum-cubes a b)
  (sum cube a inc b))

(sum-cubes 1 10)
|#

;; 1.29
(define cube
  (lambda (x) (* x x x)))

(define quad
  (lambda (x) (* x x x x)))

(define (h a b n)
  (/ (- b a) n))

(define (f term a k h)
  (term (+ (* k h) a)))

(define (sRule a b ffunc n)
  (let ((hval (h a b n)))
  (cond ((= 1 (modulo n 2)) 0)
       (else (* (/ hval 3) (sum ffunc hval a 0 n))))))

(define (sum ffunc hval a k n)
  (cond ((> k n) 0)
        ((or (= k 0) (= k n)) (+ (f ffunc a k hval) (sum ffunc hval a (+ k 1) n)))
        ((= (modulo k 2) 1) (+ (* 4(f ffunc a k hval)) (sum ffunc hval a (+ k 1) n)))
        (else (+ (* 2(f ffunc a k hval)) (sum ffunc hval a (+ k 1) n)))))


;; 1.30
(define (sum-iter term a next b)
  (define (iter a result)
    (cond ((> a b) result)
         ((iter (next a) (+ result (term a))))))
  (iter a 0))

(define (inc x)
  (+ x 1))

;; 1.31
;---Iterative---
(define (prod-iter term a next b)
  (define (iter a result)
    (cond ((> a b) result)
         ((iter (next a) (* result (term a))))))
  (iter a 1))

(define identity (lambda (x) x))

(define square
  (lambda (x) (* x x)))

;;---Recursive---
(define (prod-rec term a next b)
  (cond ((> a b) 1)
        (else (* (term a) (prod-rec term (next a) next b)))))

;; 1.41
(define (double f)
  (lambda (x) (f (f x))))

;; 1.42
(define (compose f i)
  (lambda (x) (f (i x))))