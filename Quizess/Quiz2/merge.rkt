(define (merge n m)
  (define (m-iter n m AP)
    (cond ((zero? n) (insert m AP))
          ((zero? m) (insert n AP))
          (else (cond ((> (modulo n 10) (modulo m 10))
                       (m-iter (quotient n 10) m (insert (modulo n 10) AP)))
                      (else (m-iter n (quotient m 10) (insert (modulo m 10) AP)))))))
  (m-iter n m 0))

(define (insert d AP)
  (cond ((zero? AP) d)
        (else (+ (* d (expt 10 (num-dig AP))) AP))))

(define (num-dig x)
  (inexact->exact (floor (+ 1 (log x 10)))))

(merge 15 129)