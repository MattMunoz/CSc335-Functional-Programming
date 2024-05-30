;;Insert Sort of a number


;; --Recursive
#|
(define (insert-sort-rec x)
  (cond ((zero? (quotient x 10)) x)
        ((insert (insert-sort-rec (quotient x 10)) (modulo x 10)))))
|#

(define (insert-sort-rec n)
  (cond ((< n 10) n)
        (else (insert (insert-sort-rec (quotient n 10)) (modulo n 10)))))

(define (insert n d)
  (cond ((< d (modulo n 10)) (+ (* n 10) d))
        (else (+(*(insert(quotient n 10) d) 10) (modulo n 10)))))


;; --Iterative--
(define (insert-sort-iter x)
  (define (iter x AP)
   (cond ((zero? x) AP)
         ((iter (quotient x 10) (insert AP (modulo x 10))))))
  (iter x 0))

;; --helper functions for both--
#|
(define (insert x d)
  (cond ((> d (modulo x 10)) (form-num x d))
        (else (form-num (insert (quotient x 10) d) (modulo x 10)))))

(define (num-dig x)
  (inexact->exact (floor (+ 1 (log x 10)))))

(define (form-num x y)
  (+ (* x (expt 10 (num-dig y))) y))
|#