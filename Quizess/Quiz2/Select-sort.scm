;; Selection-sort

;; --Recursive--





;; --Interative--
(define (select-sort x AP)
  (cond ((< x 10) AP)
        (select-sort )))

;; helper functions
(define (num-dig x)
  (inexact->exact (floor (+ 1 (log x 10)))))

(define(insert-at-front x d)
  (+ (* d (expt 10 (num-dig x))) x))

(define (find-largest-insert-back x l)
  (cond ((zero? x) l)
        ((> l (modulo x 10)) (find-largest-insert-back (quotient x 10) l))
        (else (find-largest-insert-back (quotient x 10) (modulo x 10)))))