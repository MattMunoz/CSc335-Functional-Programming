-;;; Homework #4

;;; ----------Question 1----------

; 1.11)

; Recursive
(define (func-f-rec n)
  (cond ((< n 3) n)
        ((+ (func-f-rec (- n 1))
            (* 2 (func-f-rec (- n 2)))
            (* 3 (func-f-rec (- n 3)))))))

; Iterative
#|
(define (func-f n)
  (func-f-iter 2 1 0 result n))

(define (func-f-iter a b c result n)
  (cond ((< n 3) n) 
           ((<= count 0) a) 
           (else (func-f-iter (+ a (* 2 b) (* 3 c)) a b (- count 1))))
 |#

 (define (func-f n) 
   (define (func-f-iter a b c count) 
     (cond ((< n 3) n) 
           ((<= count 0) a) 
           (else (func-f-iter (+ a (* 2 b) (* 3 c)) a b (- count 1)))))
   (func-f-iter 2 1 0 (- n 2))) 


; 1.12)


;;; ----------Question 2----------

;;; Test increasing order

; Recursive
(define (increasing-rec x)
    (cond ((< x 10) #t)
          ((< (modulo x 10) (modulo (quotient x 10) 10)) #f)
          ((eqv? (increasing-rec (quotient x 10)) #t))))

; Iterative
(define (increasing-iter x)
    (cond ((< x 100) (< (quotient x 10) (modulo x 10)))
          ((< (modulo x 10) (modulo (quotient x 10) 10)) #f)
          (else (increasing-iter (quotient x 10)))))

(increasing-rec 12354)
