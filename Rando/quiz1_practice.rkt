#|----------------------------------------------
(define (rev x)
  (let ((n (num-dig x)))
    (cond ((= n 1)
           x)
          ((= n 2)
           (build-two (get-dig x 1) (get-dig x 0)))
          ((= n 3)
           (build-three (get-dig x 2) (get-dig x 1) (get-dig x 0)))
          ((= n 4)
           (build-four (get-dig x 3) (get-dig x 2) (get-dig x 1) (get-dig x 0))))))


(define (num-dig x)
  (+ 1 (inexact->exact (floor (/ (log x) (log 10))))))

(define (get-dig x n)
  (modulo (quotient x (expt 10 n)) 10))

(define (build-two x y)
  (+ x (* y 10)))

(define (build-three x y z)
  (+ x (* 10 (build-two y z))))

(define (build-four x y z r)
  (+ x (* 10 (build-three y z r))))
--------------------------------------------|#

(define (reverse4 x)
  (+ (get-digit x 3) (* 10 (get-digit x 2)) (* 100 (get-digit x 1)) (* 1000 (get-digit x 0))))

(define (get-digit x n)
  (modulo (quotient x (expt 10 n)) 10))

;-----------------------------------------------------------------

(define (3rdsmallestof5 a b c d e)
  (let ((m (smallestof5 a b c d e)))
    (cond ((= a m) (largest4 b c d e))
          ((= b m) (largest4 a c d e))
          ((= c m) (largest4 b a d e))
          ((= d m) (largest4 b c a e))
          (else (largest4 b c a d)))))

 (define (largest4 a b c d)
   (let ((m (smallestof4 a b c d)))
         (cond ((= m a) (smallestof3 b c d))
               ((= m b) (smallestof3 a c d))
               ((= m c) (smallestof3 b a d))
               (else (smallestof3 b c a)))))


(define (smallestof2 x y)
  (cond ((< x y) x)
        (else y)))

(define (smallestof3 x y z)
  (smallestof2 z (smallestof2 x y)))

(define (smallestof4 a b c d)
  (smallestof2 a (smallestof3 b c d)))

(define (smallestof5 a b c d e)
 (smallestof2 a (smallestof4 b c d e)))


