(define (partition n d)
  (define (p-iter n d GT ED LT)
    (cond ((zero? n) (insert (insert GT ED) LT))
                  ((< (modulo n 10) d) (p-iter (quotient n 10) d GT ED (insert (modulo n 10) LT)))
                  ((> (modulo n 10) d) (p-iter (quotient n 10) d (insert (modulo n 10) GT) ED LT))
                  ((= (modulo n 10) d) (p-iter (quotient n 10) d GT (insert ED (modulo n 10)) LT))))
  (p-iter n d 0 0 0))

(define (insert x y)
  (cond ((zero? y) x)
        (else (+ y (* x (expt 10 (num-dig y)))))))

(define (num-dig x)
  (inexact->exact (floor (+ 1 (log x 10)))))

(partition 9281384556 5)