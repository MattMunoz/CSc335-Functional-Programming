(define l (list 1 2 3 4 5 6 7 8 9))

; 1.  Write your own version of length using the list functions we have discussed.  You can find
; length documented at http://www.schemers.org/Documents/Standards/R5RS/

; The idea is suggested by (my-length '(a b c d)) = 4.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
#|
Pre: l is a list 

Post: return the length of the list


---Recursion---

D&C: cdr down the list through the cons backbone until we reach the empty list, process
the car explicitly by returning a 1 and adding all of these ones, the returned value from
the final retured call will be the length of the lsit

IH: the cdr of a list will itself be a list, therfore we can assume the count of the elements
to the left of the car will be returned accurately

IS: the car of that call will add and return an appropriate value to the count,
i.e.
0 is the car is the empty list,
1 otherwise

Basis: the empty list is encountered
|#

;; Guess Code
(define (len-recur l)
  (cond ((null? l) 0)
        ((+ 1 (len-recur (cdr l))))))

(len-recur l)

#|
Pre: l is a list 

Post: return the length of the list


---Iteration---

DI: cdr down the list, for every atom or list in the list increment the counter by 1,
once the empty list is encountered return the count

GI
---------------------------------------------------------------
l = [ap][nyp]

nyp = [car l][cdr l] 

ap = elements not in (cdr l) and that are not (car l) ---> also current count,
which is all of the atoms and lists encountered so far

Termination: the empty list is encountered

---------------------------------------------------------------
|#


;; Guess Code
(define (len-iter l)
  (define (iter l count)
    (cond ((null? l) count)
          ((iter (cdr l) (+ 1 count)))))
  (iter l 0))

(len-iter l)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
#|
Pre: l is a list, and nth value indicating a position in the list

Post: return the element in the nth position of the list, where the list is zero indexed


---Recursion---

D&C: 

IH:

IS:

Basis:


( 1 2 3 |4| 5 6 7 8 9 () )
  | | |  |  | | | | |
  0 1 2  3  4 5 6 7 8 

|#

; Guess Code


;;; use len as a helper function



#|
---Iteration---

DI: cdr down the list, decrement the nth number by one for every car enounterend in the list,
once the nth number returns is equal to 0 return the car at that time. If the nth value ver become is positive when the empty list is enountered return the empty list

GI
---------------------------------------------------------------
n = [ap][nyp]

nyp = [car l][cdr l] 

ap = elements not in (cdr l) and that are not (car l)

count = n - # of elements in ap

Termination: the empty list is encountered

---------------------------------------------------------------
|#

;; Guess Code
(define (list-ref-iter l n)
    (cond ((null? l) '())
          ((zero? n) (car l))
          (else (list-ref-iter (cdr l) (- n 1)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
#|
Pre: l is a list, and nth value indicating a position in the list

Post: return the element in the nth position of the list, where the list is zero indexed


---Recursion---

D&C: remove the car of the list on each call and hold it in that call, when the call
is returned car that element into the new list. on each call the n will be decremented by 1.
Additionally, the elements returned by the recursive call will be maintained in their original order

IH: We assume the returned value will be a list containeg the (n - current index) elements
in the original list l

IS: The current element in the current call will be car'd into the new forming list

Basis: either n = 0 or the empty list is encountered

|#

; Guess Code
(define (recur l n)
  (cond ((null? l) '())
        ((zero? n) '())
        (else (cons (car l) (recur (cdr l) (- n 1))))))


#|
---Iteration---

DI: cdr down the list, decrement the nth number by one for every car enounterend in the list, add the car of that element to a new list, once the nth value reaches zero return the newly created list

GI
---------------------------------------------------------------
n = [ap][nyp]

nyp = [car l][cdr l] 

ap = elements not in (cdr l) and that are not (car l)

Termination: n = 0

---------------------------------------------------------------
|#

;; Guess Code
(define (first-num-iter l n)
  (define (iter l new-l n)
    (cond ((null? l) new-l)
          ((zero? n) new-l)
          (else (iter (cdr l) (cons (car l) new-l) (- n 1)))))
  (reverse (iter l '() n)))


;;; only other thing i can think of is create a proper append function and append
;;; to the list using that helper funciton

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;






;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
#|
Pre: 

Post: 


---Recursion---

D&C: 

IH: 

IS: 

Basis: 

|#

; Guess Code


#|
---Iteration---

DI: cdr down the list, decrement the nth number by one for every car enounterend in the list, add the car of that element to a new list, once the nth value reaches zero return the newly created list

GI
---------------------------------------------------------------
n = [ap][nyp]

nyp = [car l][cdr l] 

ap = elements not in (cdr l) and that are not (car l)

Termination: n = 0

---------------------------------------------------------------
|#

;; Guess Code
(define (first-num-iter l n)
  (define (iter l new-l n)
    (cond ((null? l) new-l)
          ((zero? n) new-l)
          (else (iter (cdr l) (cons (car l) new-l) (- n 1)))))
  (reverse (iter l '() n)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;




