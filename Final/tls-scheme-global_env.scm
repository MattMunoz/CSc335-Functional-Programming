; CSc 335, Spring 2024 Final
;;; Name: Matthew Munoz, Jezlea Ortega

;;; Spring 2024 Take-home Final Question #2 (TLS with global environment)


#|
TLS with global scope:
The global scope variant of TLS works by intializing a global scope table for reference within any function. More specifically, this table is a single table that persists throught program, so any lookup in the table is done within the same table for all functions and closures.


---PROOF---

Basis: Number, vooleans, and primative all belong to TLS so by our assumptino they are all corrently evaluated

IH: The global environment table created upon intialization of the program persists and is the same one being reference upon each lookup.

IS: Upon declaring a closure a new entry is created for the table via the "rib", whereby the entry consists of two lists, one being the formals and the other being the bindings to those formals. We can assume that all actions prior to reaching myapply-closure works as base TLS does. So, when we reach that part of the implementation a new entry is created for that closure. However, this entry will be placed upon the prexisting global environment that is referencable thorughout the entierty of the program. This "new" environment is then assigned to the global environemnt via the assignment operator, where a function can then refernce the newly created environment via the meaning fucntion call. To ensure the gloabl environemnt stay conssitent with lexical scoping and that the variable bindings are not accessible outside of their calling, the global environemnt is set to its previous version prior to the closure that was created.
|#


(define global-env '())

; auxiliary functions

(define build
  (lambda (s1 s2)
    (cons s1 (cons s2 (quote ())))))

(define first car)

(define second cadr)

(define third caddr)

(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))



; environments implemented as tables
(define lookup-in-table
  (lambda (name global table-f)
    (cond 
      ((null? global) (table-f name))
      (else (lookup-in-entry name
                             (car global)
                             (lambda (name)
                               (lookup-in-table name
                                                (cdr global)
                                                table-f)))))))

(define extend-table cons)



(define lookup-in-entry
  (lambda (name entry entry-f)
    (lookup-in-entry-help name
                          (names entry)
                          (vals entry)
                          entry-f)))



(define lookup-in-entry-help
  (lambda (name names vals entry-f)
    (cond
      ((null? names) (entry-f name))
      ((eq? (car names) name) (car vals))
      (else (lookup-in-entry-help name
                                  (cdr names)
                                  (cdr vals)
                                  entry-f)))))




(define new-entry build)

(define names
  (lambda (entry) (car entry)))

(define vals
  (lambda (entry) (cadr entry)))




; the top level of the interpreter

(define value
  (lambda (e)
    (meaning e)))


(define meaning
  (lambda (e)
    ((expression-to-action e) e)))


; supporting functions for the intepeter

; syntax-directed dispatch on expression

(define expression-to-action
  (lambda (e)
    (cond 
      ((atom? e) (atom-to-action e))
      (else (list-to-action e)))))

(define atom-to-action
  (lambda (e)
    (cond
      ((number? e) *const)
      ((eq? e #t) *const)
      ((eq? e #f) *const)
      ((eq? e (quote cons)) *const)
      ((eq? e (quote car)) *const)
      ((eq? e (quote cdr)) *const)
      ((eq? e (quote null?)) *const)
      ((eq? e (quote eq?)) *const)
      ((eq? e (quote atom?)) *const)
      ((eq? e (quote zero?)) *const)
      ((eq? e (quote add1)) *const)
      ((eq? e (quote mul)) *const)
      ((eq? e (quote sub1)) *const)
      ((eq? e (quote number?)) *const)
      (else *identifier))))


(define list-to-action
  (lambda (e)
    (cond
      ((atom? (car e))
       (cond 
         ((eq? (car e) (quote quote))
          *quote)
         ((eq? (car e) (quote lambda))
          *lambda)
         ((eq? (car e) (quote cond))
          *cond)
         (else *application)))
      (else *application))))


; operational semantics -- the definitions of the action functions

(define *const
  (lambda (e)
    (cond 
      ((number? e) e)
      ((eq? e #t) #t)
      ((eq? e #f) #f)
      (else (build (quote primitive) e)))))


(define *quote
  (lambda (e)
    (text-of e)))

(define text-of second)


(define *identifier
  (lambda (e)
    (lookup-in-table e global-env initial-table)))


; note that as (car (quote ())) throws an error, this definition
; amounts to saying that looking anything up in the initial table
; is impossible.
(define initial-table
  (lambda (name)
      (car (quote ()))))


(define *lambda
  (lambda (e)
    (build (quote non-primitive)
           (cons global-env (cdr e)))))

(define table-of first)

(define formals-of second)

(define body-of third)


; cond is a special form that takes any number of 
; cond-lines ...  if it sees an else-line, it treats
; that cond-line as if its question part were true.

(define evcon
  (lambda (lines)
    (cond 
      ((else? (question-of (car lines)))
       (meaning (answer-of (car lines))))
      ((meaning (question-of (car lines)))
       (meaning (answer-of (car lines))))
      (else (evcon (cdr lines))))))


(define else?
  (lambda (x)
    (cond 
      ((atom? x) (eq? x (quote else)))
      (else #f))))

(define question-of first)

(define answer-of second)



(define *cond 
  (lambda (e)
    (evcon (cond-lines-of e))))

(define cond-lines-of cdr)


(define evlis
  (lambda (args)
    (cond 
      ((null? args) (quote ()))
      (else
       (cons (meaning (car args))
             (evlis (cdr args)))))))



(define *application
  (lambda (e)
    (myapply
     (meaning (function-of e))
     (evlis (arguments-of e)))))

(define function-of car)

(define arguments-of cdr)




(define primitive?
  (lambda (l)
    (eq? (first l) (quote primitive))))

(define non-primitive?
  (lambda (l)
    (eq? (first l) (quote non-primitive))))



(define myapply
  (lambda (fun vals)
    (cond
      ((primitive? fun)
       (myapply-primitive
        (second fun) vals))
      ((non-primitive? fun)
       (myapply-closure
        (second fun) vals)))))


(define myapply-primitive
  (lambda (name vals)
    (cond
      ((eq? name (quote cons))
       (cons (first vals) (second vals)))
      ((eq? name (quote car))
       (car (first vals)))
      ((eq? name (quote cdr))
       (cdr (first vals)))
      ((eq? name (quote null?))
       (null? (first vals)))
      ((eq? name (quote eq?))
       (eq? (first vals) (second vals)))
      ((eq? name (quote atom?))
       (:atom? (first vals)))
      ((eq? name (quote zero?))
       (zero? (first vals)))
      ((eq? name (quote add1))
       ((lambda (x) (+ x 1)) (first vals)))
      ((eq? name (quote mul))
       (* (first vals) (second vals)))
      ((eq? name (quote sub1))
       ((lambda (x) (- x 1)) (first vals)))
      ((eq? name (quote number?))
       (number? (first vals))))))

(define :atom?
  (lambda (x)
    (cond 
      ((atom? x) #t)
      ((null? x) #f)
      ((eq? (car x) (quote primitive))
       #t)
      ((eq? (car x) (quote non-primitive))
       #t)
      (else #f))))

(define myapply-closure
  (lambda (closure vals)
    (let ((temp-env (extend-table
                     (new-entry
                      (formals-of closure)
                      vals)
                     global-env)))
      (set! global-env temp-env)
      (let ((local (meaning (body-of closure))))
        (set! global-env (cdr temp-env))
        local
        ))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
#|
Changes to scoping:
The gloabl environment interpretation of scoping whorks in musch the same way that TLS works. However, the ky difference in this interpretation is the lookup in global scope anytime that a variable is needed. An example can be seen below.
|#
;;; Example of different interpreation
(value '((lambda (x y)
           (y))
         10
         ((lambda (x)
            (lambda () x))
          5)))

; In base TLS this function returns 5 instead of the global scope implemenation which returns 10

#|
The implemetnation creates a binding for both x and y, with x being bound to 10 and y being bound to ((lambda (x) (lambda () x)) 5). This creates a closure where x is bound to 5 . The difference in the two interpreters comes in the way of looking up via their scope. In this version, the lookup happens via the gloabal environment, where x is bound to 10. In the base TLS interpreter the lookup comes via the local scope where x is bound to 5.  
|#
