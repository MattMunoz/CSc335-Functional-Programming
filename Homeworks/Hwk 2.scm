;;; Homework #2

; ---Problem 1---
(cond ((= 5 6)) ((> 8 7))
      (else #f))


; ---Problem 2---
(cond ((= 2 3) #f)
      (else #t))

;; ---Problem 3---
; A) 1.7
; -- Fails for small numbers since the variation is already smaller than .0001 the test will stop well before the number is close to its accurate number
; -- For large numbers the system may never stop beacuse the square of the guess may never be within .0001
(define (sr x)
  (sqrt-iter 1.0 x))

(define (square x) (* x x))

(define (sqrt-iter guess x)
  (if (good-enough? guess x)
      guess
      (sqrt-iter (improve guess x)
                 x)))

(define (improve guess x)
  (average guess (/ x guess)))

(define (average x y)
  (/ (+ x y) 2))

;(define (good-enough? guess x)
; (< (abs (- (square guess) x)) 0.001))


; Changed the good-enough from a purly subtraction based to division to check the prescision of the guess
(define (good-enough? guess x)
  (and (> (/ (square guess) x) .99999) (< (/ (square guess) x) 1.00001)))

; B) 1.8
(define (cr x)
  (cube-iter 1.0 x))

(define (cube x) (* x x x))

(define (cube-iter guess x)
  (if (good-enough-cu? guess x)
      guess
      (cube-iter (improve-cu guess x)
                 x)))

(define (improve-cu guess x)
  (/ (+ (/ x (square guess))(* 2 guess)) 3))

(define (good-enough-cu? guess x)
  (and (> (/ (cube guess) x) .9999999999999) (< (/ (cube guess) x) 1.0000000000001)))


;; ---Problem 4---
; In notebook

;; ---Problem 5---
#| (define (sortfive a b c d e)
  (let ((l (list)) (fi (five a b c d e))
                   (on (one a b c d e)))
    (let ((fo (four a b c d e fi)) (tw (two a b c d e on)))
      (let ((th (three a b c d e on tw fo fi)))
        (display (cons on (cons tw (cons th (cons fo (cons fi l)))))))
      )
    ))

(define (big x y)
  (cond ((> x y) x) (else y)))

(define (small x y)
  (cond ((< x y) x) (else y)))

(define (five a b c d e)
  (big (big (big a b) (big c d)) e))

(define (one a b c d e)
  (small (small (small a b) (small c d)) e))

(define (four a b c d e l)
  (cond ((= l a) (big (big b c) (big d e)))
        ((= l b) (big (big a c) (big d e)))
        ((= l c) (big (big a b) (big d e)))
        ((= l d) (big (big a b) (big c e)))
        ((= l e) (big (big a b) (big c d)))))

(define (two a b c d e l)
  (cond ((= l a) (small (small b c) (small d e)))
        ((= l b) (small (small a c) (small d e)))
        ((= l c) (small (small a b) (small d e)))
        ((= l d) (small (small a b) (small c e)))
        ((= l e) (small (small a b) (small c d)))))

(define (three a b c d e on tw fo fi)
  (- (+ a b c d e) (+ on tw fo fi)))
|#

(define (sorttwo x y)
  (cond ((> x y) (list y x)) ((list x y))))

(define (sortthree x y z)
  (let ((m (min-three x y z)))
  (cond ((= m z) (cons z (sorttwo x y)))
        ((= m y) (cons y (sorttwo z x)))
        (else (cons x (sorttwo y z))))))

(define (sortfour a b c d)
  (let ((m (min-four a b c d)))
  (cond ((= m b) (cons b (sortthree a c d)))
        ((= m c) (cons c (sortthree a b d)))
        ((= m d) (cons d (sortthree a b c)))
        (else (cons a (sortthree b c d))))))

(define (sortfive a b c d e)
  (let ((m (min-five a b c d e)))
  (cond ((= m e) (cons e (sortfour a b c d)))
        ((= m b) (cons b (sortfour a c d e)))
        ((= m c) (cons c (sortfour a b d e)))
        ((= m d) (cons d (sortfour a b c e)))
        (else (cons a (sortfour b c d e))))))

(define (min-two x y)
  (cond ((< x y) x)
        (else y)))

(define (min-three x y z)
  (min-two z (min-two x y)))

(define (min-four a b c d)
  (min-two d (min-three a b c)))

(define (min-five a b c d e)
  (min-two e (min-four a b c d)))





