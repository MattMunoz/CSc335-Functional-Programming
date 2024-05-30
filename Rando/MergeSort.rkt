(define (mergesort n)
  (cond ((< n 10) n)
        (else  (let ((l1 (split-even n)) (l2 (split-odd n)))
                 (merge (mergesort l1) (mergesort l2))))))

(define (split-even n)
  (cond ((< n 10) n)
        (else (+ (* (split-even (quotient n 100)) 10) (modulo n 10)))))

(define (split-odd n)
  (cond ((< n 100) (modulo (quotient n 10) 10))
        (else (+ (* (split-odd (quotient n 100)) 10) (modulo (quotient n 10) 10)))))

(define (merge l1 l2)
  (define (merge-iter l1 l2 n)
    (cond ((zero? l1) (combine l2 n))
          ((zero? l2) (combine l1 n))
          ((> (modulo l1 10) (modulo l2 10)) (merge-iter (quotient l1 10) l2 (combine (modulo l1 10) n)))
          (else (merge-iter l1 (quotient l2 10) (combine (modulo l2 10) n)))))
  (quotient (merge-iter l1 l2 0) 10))

(define (combine x y)
  (+ (* x (expt 10 (num-dig y))) y))

(define (num-dig n)
  (define (num-dig-iter n count)
    (cond ((< n 10) (+ 1 count))
          (else (num-dig-iter (quotient n 10) (+ 1 count)))))
  (num-dig-iter n 0))