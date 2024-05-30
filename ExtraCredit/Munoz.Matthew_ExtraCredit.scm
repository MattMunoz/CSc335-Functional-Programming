;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Name: Matthew Muonz
; Email: mmunoz005@citymail.cuny.edu

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


#|

LD-count development:

Pre: n is an LD number, meaning the leftmost digit is a 1 and the rightmost digit is a 2 with at least 1 other digit in between.

Post: The count of LD numbers that appear inside the LD number, meaning all of the possible LD numbers that can be formed from the given LD number.

Idea:

The code will first remove the leftmost and rightmost digit of n, via the use of a helper function rlr, since we are only concerned with the different possibilities inside the LD number. However, a 1 will be added at the end for the number n where the outer digits don't pair, for which there are no possible inner pairs.

The new number will then be scanned from right to left, one digit at a time looking for any potential open paren., in this case, a 2. The scan can be completed via a recursive call which skips all numbers that are not a 2 since those digits cannot have a pair and therefore will not have an effect on possible LD's. If a 2 is encountered, the call will create a recursive call that adds the possibility of that 2 finding a pair and that 2 not finding a pair. The non-pair call will be done by continuing the scan to the right of that 2, while the pair of 2 will call an iterative function that looks left for any possible 1 it can pair with. This sum of possible pairs for that 2 will return the count of all possible LD's capable, including counting it as a paren and as a digit.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
LD-rec

Pre: n > 0

Post: The number of LDs in n is returned.

D&C: The n will be scanned from right to left, removing the rightmost digit of the number n in each call. This will return a new number n that is smaller than n by one digit, meaning we are only concerned with counting the LD's in that new smaller n. This will continue until the last digit of n is encountered, since the number n can no longer be divided or a possible LD being found with just one digit.

IH: We will assume the recursive call works if the precondition holds. This would mean that the count of all possible LD to the left of the rightmost digit will be returned.

IS: The rightmost digit of that call can be identified as being able to create LD's or not, incrementing the count if LD's are possible. This pair can be done via a call to the iterative function that counts LD's and a recursive function that skips that 2, the two possibilities of 2, whether a paren or a digit.

Basis: n < 10

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
LD

Pre: n >= 0

Post: The number of LD's possible with the closing paren (2) that made the call.

DI: Since this call is made by a closing paren (2) on the right, we are only concerned with finding the open parens (1) on the left that can form a pair. However, if a 1 is found we must consider the possibility of that 1 closing and any other LD's inside that pair and outside that pair. Those can be found by calling a function to count the LDs inside that pair and outside that pair independently.

GI:
------------------------------------------------------------------------------
   - N = [nyp][rightmost digit of nyp][ap]
   - inside-LD = ap
   
   - (rightmost digit of nyp) = 1 ----> count = LD's in nyp + LD's ap
   - otherwise -----------------------> count = count

Strong: Yes, at the end of the calls all of the digits will be in nyp, with the exception of the rightmost digit of nyp. The number of LD's that can be created from ap will be the total number of LD's that can be created from N since no more LD's can be created from nyp.

Weak: Yes, since all of the digits in n will be in nyp, the total number of LD's that can be found in n will be the number of LD's in nyp. Additionally, ap will be empty meaning that no LD's can be made from it.

Preserved: Yes, all of the LD's in n will be the summation of the LD's found in ap and nyp when the rightmost digit of nyp is 1 since that will be a new possibility for the 2 that made the call to pair. Additionally, if a 1 is not encountered there are no new LD's that can be found, hence the count stays the same.

Termination: nyp is empty
------------------------------------------------------------------------------

***next variable was added to allow for the gi to hold for the rightmost digit of nyp

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
Helpers

;;;;;;;;; Insert ;;;;;;;;;
The number of digits in the y number is calculated via the helper function num-dig. This will allow x to be placed to the left of the number of digits in y, so when we add the numbers [x][y] is formed.

;;;;;;;;; num-dig ;;;;;;;;;
Pre: n > 0

Post: the number of digits is returned.

DI: Remove the rightmost digit in n one at a time, whenever a digit is removed increment a counter by 1. When n < 10 a final 1 will be added to count since there is one digit remaining.

GI:
------------------------------------------------------------------------------
   - N = [nyp][ap]
   - num-dig N = (num-dig nyp) + (count of ap)

Strong: Yes, when we end all of the digits will be in ap and therefore the count of n will equal the count in ap.

Weak: Yes, at the beginning all of the digits will be in nyp and none in ap. This would mean that the count will be 0.

Preserved: Yes, at any intermediate step the count in ap will be the number of processed digits from n.

Termination: n < 10
------------------------------------------------------------------------------

;;;;;;;;; rlr ;;;;;;;;;
Pre: n >= 0

Post: a new number with the rightmost and leftmost digit of n removed.

DI: The procedure will begin by removing the rightmost digit of n and continuing to process every digit thereafter. The removal of that digit will be done via a wrapper function not the iterative calls. The remaining processed digits will be done one at a time, being placed into a new variable until n < 10. This will be done because we do not want to include the last digit into our new-n so it will never be processed.

GI:
------------------------------------------------------------------------------
   - N = [nyp][ap]
   - new-n = ap

Strong: Yes, at the end, the digits in ap will be the processed digits which are all of the digits in N, not including the leftmost digit.

Weak: Yes, to start, new-n will be empty (i.e., 0) and ap will also be empty since all of the digits will be in nyp.

Preserved: Yes, at any intermediate step in the process, the digits in ap will equal new-n.
------------------------------------------------------------------------------

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
|#

(define (LD-count n)
    (+ 1 (LD-rec (rlr n))))
  
  (define (LD-rec n)
    (let ((q (quotient n 10)))
      (cond ((< n 10) 0)
            ((= (modulo n 10) 2) (+ (LD-rec q) (LD q)))
            (else (LD-rec q)))))

  (define (LD n)

    (define (LD-iter n next count inside-LD)
      (let ((q (quotient n 10)) (next-next (modulo (quotient n 10) 10)))
        (cond ((zero? n) count)
              ((= next 1) (LD-iter q next-next
                                   (+ 1 count (LD-rec inside-LD) (LD-rec q))
                                   (insert next inside-LD)))
              (else (LD-iter q next-next count (insert next inside-LD))))))

    
    (LD-iter (quotient n 10) (modulo (quotient n 10) 10) 0 (insert (modulo n 10) 0)))


;;;----------Helper----------
(define (insert x y)
  (+ (* x (expt 10 (num-dig y))) y))

(define (num-dig x)
  (define (num-dig-iter x count)
    (cond ((zero? x) 0)
          ((< x 10) (+ 1 count))
          (else (num-dig-iter (quotient x 10) (+ 1 count)))))
  (num-dig-iter x 0))

(define (rlr n)
  (define (remove-left-right n new-n)
    (cond ((< n 10) new-n)
          (else (remove-left-right (quotient n 10) (insert (modulo n 10) new-n)))))
    (remove-left-right (quotient n 10) 0))


;;;----------Tests----------
(LD-count 181121322156122)
(LD-count 1111122222)
(LD-count 10101012020202)
(LD-count 111152222)
(LD-count 11111522222)
(LD-count 1111122222)
(LD-count 121224232)
(LD-count 11022)
(LD-count 11822)
(LD-count 1122)