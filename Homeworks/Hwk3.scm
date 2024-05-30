;;; Homework 3

;;; ----------Question 1----------

(define (my-plus-version-1 a b)
  (if (= a 0)
      b
      (inc (my-plus-version-1 (dec a) b))))

(define (my-plus-version-2 a b)
  (if (= a 0)
      b
      (my-plus-version-2 (dec a) (inc b))))

;; where

(define (inc x) (+ x 1))

(define (dec x) (- x 1))

(my-plus-version-1 4 5)
(my-plus-version-2 4 5)



#|
First:

(my-plus-version-1 4 5)
(inc (my-plus-version-1 3 5))
(inc (inc (my-plus-version-1 2 5)))
(inc (inc (inc (my-plus-version-1 1 5))))
(inc (inc (inc (inc (my-plus-version-1 0 5)))))
(inc (inc (inc (inc 5))))
(inc (inc (inc 6)))
(inc (inc 7))
(inc 8)
9


The first procedure is the recursice call because it will require the use of the stack. Additionally, the sum is only produced after it returns all of the procedures calls from the stack

(my-plus-version-2 4 5)
(my-plus-version-2 3 6)
(my-plus-version-2 2 7)
(my-plus-version-2 1 8)
(my-plus-version-2 0 9)

The second procedure is done iteratively, since each call will caculate its own result and send that to the next call. Similarly, the stack will never have to be used and the result will immediatly be returned once the stopping condtion is met

|#


;;; ----------Question 2----------

; Recursive
(define (sum-dig-rec x)
  (cond ((zero? x) 0)
        ((+ (modulo x 10) (sum-dig-rec (quotient x 10))))))

#|

Conditions:
     Pre: A non negative integer is given as an argument to the procedure

     Post: The summation of all of the digits in a non negative integer. The nuber produced will be a nonnegative integer itself

- The number given is first checked to be 0, tb
|hat will also be the stopping condition

- If the number is not zero the last digit in the number is extracted by using (modulo) procedure

- The number in the next call is produced by removing the last digit using the (quotient) procedure

-This proces is continued until we reach the stopping cundion mentionied in the first bullet point
|#

; Iterative
(define (sum-dig-iter x result)
  (cond ((zero? x) result)
        ((sum-dig-iter (quotient x 10) (+ result (modulo x 10))))))

(define result 0)

#|

Conditions:
     Pre: A non negative integer and a result parameter defined as 0 are given to the procedure

     Post: The summation of all of the digits in a non negative integer. The nuber produced will be a nonnegative integer itself

- The number given is first checked to be 0, that will also be the stopping condition

- If the number is not zero the last digit in the number is extracted by using (modulo) procedure ans summed to the result variable

- The number in the next call is produced by removing the last digit using the (quotient) procedure and the newly accumulated result 

-This proces is continued until we reach the stopping condion mentionied in the first bullet point
|#