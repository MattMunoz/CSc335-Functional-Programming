; CSc 335, Spring 2024 Project
;;; Name: Matthew Munoz, Jezlea Ortega

;;; Spring 2024 Project Question #1 (list of LD's)

#|
Pre: an LD number is input, by definition an LD is a number containing at least 3 digits with a leading 1 and trailing 2

Post: A list of lists containing all combinations of LD/s within the larger input LD

Idea:
starting from the left search for any element equal to 1 by recursively looking at the subsequent elements one at a time. If a 1 is encountered a call will be made to search for any possible 2's that can pair with it. If a pairing 2 is encountered it indicates that an LD has been found. However, their is still the possibility of another 2 following that mating and any LDs possible within that mating as well. So, there are three possible combinations when a pair is encountered.

1. Assume that it is the only pair possible and return that list with all remaining elements appended
2. That pair is ignored and we search for any other pairing 2's following that 2
3. We create that pair and search for any remaining LD's inside other that LD and outside that LD

all of these will create individual lists that will be appended together to create all possible combinations of LD's
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
---Find 1---
Induction on the cons cells of l

D&C: cdr down the list searching for any 1 that can be the start of a new LD. This will create a smaller LD by one element on each subsequent call, leading to termination. This will continue until we have cdred down the list and encountered the '() terminating condition.

Basis: nil(i.e. '()) will be encountered, meaning we have cdred down the entire list. 

IH: all of the different combinations of LD's to the right of that element have been found and properly appended to each different combination of LD

IS: The element will be appended to every different combination of LD's found to the right of said element, thereby creating a list that includes all of the elements from the original LD in its original order. 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
---Find 2---
Design Idea: an element equal to 1 is the one that creates the call, therefore all of the 2s that are NOT the first element in the given list are mating partners for that 1. So, an initial removal of the leftmost element in this call will be done to preserve the condition that any mating 2 must NOT be the following element. Additionally, if a 2 is found that can mate with the calling 1, then we must search for all of the possible LDs containing that mating and not containing that mating. We can then decompose this into three conditions as mentioned in the greater design idea.

- it mates and we search for any remaining ld inside and outside the mating ld
- it does not mate and we search for any other mating partners after that 2
- it mates and we simply append all of the remaining elements of that mating

GI:

find-2(lst) = (append
              (append(LD-inside, remaining elements))
              (append (all previous elements, LD's after current elements))
              (append (LD's inside mating, LD's after mating))
               all reaming elemtns)


Strong: Yes, at the end of the call all of the possible mating option for 1 will be found. This is beacuse when the empty list is encountered there are no more possible mating options for the 1 that created the call. Additinally, for any 2 that is mated their is the posisibilty of finding Lds inside and outside and appendding the remaing elemets, which could be the empty list if the last element before the empty list is a 2.

Weak: To start, none of the elements to left of the leftmost list that made the call have been processed, therefore the lst is still complete. Additionally, none of the LD's partaining to that calling 1 have been found so the remaining list is the original calling list with all elements in the same order

Preservable/Intermediate: at any point in the call the mating 2 before the current elements have been found and all of the remaining(unprocessed) elements are left to find a mate. All of the found mates include all of the different combinations that the mating can happen. So, a look inside any LD will return all LD's inside of it while also attempting to find all LD's after it.

Termination: the empty list is encountered 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
|#

(define (LD-lst l)
  (let ((r (rlr-lst (listify l))))
    (append (find-1 r) (cons r '()))))

(define (find-1 l)
  (cond ((null? l) '())
        ((and (not (null? (cdr l))) (eq? (car l) 1))  (append (append-to-each-right '(1) (find-1 (cdr l))) (find-2 (cdr l))))
        (else (append-to-each-right (list (car l)) (find-1 (cdr l))))))

(define (find-2 l)
  
  (define (f-2 l lst up-to)
    (let ((ut (find-1 up-to)))
      (cond ((null? l) lst)
            ((eq? (car l) 2)
             (f-2 (cdr l) (append
                           (append-to-each-left (cons-each ut) (cdr l))
                           (append-to-each-right (cons up-to '()) (find-1 (cdr l)))
                           (accumulate append '() (outer ut (find-1 (cdr l))))
                           (cons (cons up-to (cdr l)) lst)) (append up-to (list (car l)))))
            (else (f-2 (cdr l) lst (append up-to (list (car l))))))))

  (f-2 (cdr l) '() (list (car l))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;Helpers;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; ---Setup input as list---

#|
listify
Pre: a non zero number is given

Post: a list where each digit is converted to an entry in the list

induction on length of non-negative input

Each digit will be removed one at a time from the right via the modulo operator until n < 10, at which point a list containing the leftmost digit is returned. Then each removed digit will be consed onto to the end of the returned list
|#

(define (listify n)
  (cond ((< n 10) (cons n '()))
        (else (append (listify (quotient n 10)) (cons (modulo n 10) '())))))


#|
rlr-lst (remove-left-right of list)

a list will be given as input.

the leftmost element of the list will be removed from the given list via the rl-lst function. This is accomplished through the cdr which returns a list with the leftmost element removed.

To remove the rightmost element, the list is first removed before performing a cdr procedure to remove the leftmost(originally, rightmost) element of the list. The list is then reversed to ensure all elements are in the original order with the left and right most elements removed.
|#
(define (rlr-lst l)
  (rl-lst (rr-lst l)))

(define (rr-lst l)
  (reverse (cdr (reverse l))))

(define (rl-lst l)
  (cdr l))


;;; ---Form LD by appending and consing the correct side of the remaining LD---

#|
a lat and a list of lists is given to append-each-right. We will induct on the length of l2, cdring down the list one at a time. A new list will be returned with the list l1 being appended to the left of each of the elements in the list. The return value will have the same cardinality as l2

Similar to append-each-right, append-each-left will return a list of lists with equal cardinality to l2. However the appended l1 will be appended to the left of each element in l2.

cons each will cdr down the elements in l and cons each one to simply create the parentheses of each 1-2 combination for easier understanding and ld number seperation within the greater ld number.
|#
(define (append-to-each-right l1 l2)
  (cond ((null? l2) '()) 
        (else (cons (append l1 (car l2)) (append-to-each-right l1 (cdr l2))))))

(define (append-to-each-left l1 l2)
  (cond ((null? l1) '())
        (else (cons (append (car l1) l2) (append-to-each-left (cdr l1) l2)))))

(define (cons-each l)
  (cond ((null? l) '())
        (else (cons (cons (car l) '()) (cons-each (cdr l))))))


;;; ---Used to find all LDs within an already formed LD and after the formed LD---

#|
accumulate proof given in class, no further explanation needed as per notes.

outer/innner takes two lists and returns a list of lists with each element in i consed onto every element in j. The returned list of lists will be of cardinality i*j whereby each element in i will be consed to the left of the lists in j.
|#
(define (accumulate op init seq)
  (cond ((null? seq) init)
        (else (op (car seq) (accumulate op init (cdr seq))))))

(define (outer i j)
  (cond ((null? i) '())
        (else (cons (inner (car i) j) (outer (cdr i) j)))))

(define (inner i j)
  (cond ((null? j) '())
        (else (cons (list i (car j)) (inner i (cdr j))))))


(LD-lst 11122111222111122222)
(display "length: ")
(display (length (LD-lst 11122111222111122222)))
(newline)

(newline)
(LD-lst 181121322156122)
(display "length: ")
(display (length (LD-lst 181121322156122)))
(newline)

(newline)
(LD-lst 1111122222)
(display "length: ")
(display (length (LD-lst 1111122222)))
(newline)

(newline)
(LD-lst 10101012020202)
(display "length: ")
(display (length (LD-lst 10101012020202)))
(newline)

(newline)
(LD-lst 111152222)
(display "length: ")
(display (length (LD-lst 111152222)))
(newline)

(newline)
(LD-lst 11111522222)
(display "length: ")
(display (length (LD-lst 11111522222)))
(newline)

(newline)
(LD-lst 121224232)
(display "length: ")
(display (length (LD-lst 121224232)))
(newline)

(newline)
(LD-lst 11022)
(display "length: ")
(display (length (LD-lst 11022)))
(newline)

(newline)
(LD-lst 11822)
(display "length: ")
(display (length (LD-lst 11822)))
(newline)

(newline)
(LD-lst 1122)
(display "length: ")
(display (length (LD-lst 1122)))
(newline)