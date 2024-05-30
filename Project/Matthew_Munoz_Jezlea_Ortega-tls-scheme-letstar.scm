; CSc 335, Spring 2024 Project
;;; Name: Matthew Munoz, Jezlea Ortega

;;; Spring 2024 Project Question #3 (TLS extending let*)


; first scheme interpreter


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; tls-scheme, from chapter 10 of tls

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; need to allow redefinition of initial bindings in r5rs as delivered
; by drracket

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



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
  (lambda (name table table-f)
    (cond 
      ((null? table) (table-f name))
      (else (lookup-in-entry name
                             (car table)
                             (lambda (name)
                               (lookup-in-table name
                                                (cdr table)
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
    (meaning e (quote () ))))


(define meaning
  (lambda (e table)
    ((expression-to-action e) e table)))


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
         ((eq? (car e) (quote let*))
          *let*)
         (else *application)))
      (else *application))))

; operational semantics -- the definitions of the action functions

(define *const
  (lambda (e table)
    (cond 
      ((number? e) e)
      ((eq? e #t) #t)
      ((eq? e #f) #f)
      (else (build (quote primitive) e)))))


(define *quote
  (lambda (e table)
    (text-of e)))

(define text-of second)


(define *identifier
  (lambda (e table)
    (lookup-in-table e table initial-table)))


; note that as (car (quote ())) throws an error, this definition
; amounts to saying that looking anything up in the initial table
; is impossible.
(define initial-table
  (lambda (name)
      (car (quote ()))))


(define *lambda
  (lambda (e table)
    (build (quote non-primitive)
           (cons table (cdr e)))))

(define table-of first)

(define formals-of second)

(define body-of third)


; cond is a special form that takes any number of 
; cond-lines ...  if it sees an else-line, it treats
; that cond-line as if its question part were true.

(define evcon
  (lambda (lines table)
    (cond 
      ((else? (question-of (car lines)))
       (meaning (answer-of (car lines))
                table))
      ((meaning (question-of (car lines))
                table)
       (meaning (answer-of (car lines))
                table))
      (else (evcon (cdr lines) table)))))


(define else?
  (lambda (x)
    (cond 
      ((atom? x) (eq? x (quote else)))
      (else #f))))

(define question-of first)

(define answer-of second)



(define *cond 
  (lambda (e table)
    (evcon (cond-lines-of e) table)))

(define cond-lines-of cdr)



(define evlis
  (lambda (args table)
    (cond 
      ((null? args) (quote ()))
      (else
       (cons (meaning (car args) table)
             (evlis (cdr args) table))))))

(define *application
  (lambda (e table)
    (myapply
     (meaning (function-of e) table)
     (evlis (arguments-of e) table))))

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
       (sub1 (first vals)))     
;;;; deliberate error: ask class to figure out how to repair it.  
      
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
    (meaning (body-of closure)
             (extend-table
              (new-entry
               (formals-of closure)
               vals)
              (table-of closure)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;PROOF:

;Under the assumption that TLS without let* correctly evaluates the given quoted procedure, we can develop a TLS program extended by let*. We will first expand
;list-to-action to go to let*'s respective function if (eq? (car e) (quote let*)), because we have identified the presence of a let* procedure. Then, we will need another function
;*let* which interprets what occurs if we recognize a let*. If we have let* we will need to refer back to our meaning function the environment of let* to build the table. As such,
;similar to the concept of nested lets will create a new function which makes a new environment for let*. The function will take the cadr of e which is the formals with their
;values in let*, and the body of let which we will find with caddr e. We will recursively create an environment for every binding within let* where by new closure will be created.
;Each closure will be created via a lambda with each proceeding lambda nested within the previous closures. If we have (null? formval) we simply return the body for evaluation.
;(make-let*-env( will now be returned to meaning and be our new e. Now, given we know the interpreter without let* correctly evaluates the given quoted procedures meaning will carry
;out the procedures as normal and evalute let* using the table.

;The induction will be on the number of bindings within let*.

;Basis: numbers, booleans, and primatives all belong to TLS and are correctly evaluated.

;I.H: Assume the conclusion is valid for all proper components of let*, meaning will take (make-let*-env (cdr formval) body))) (list (cadar formval)) and the table to evalute,
;     let* in the TLS interpreter.

;I.S:
; (i): The current expression is '(let* ((bindings)) (body)), where let* will be evaluated as a list-to-action calling upon *let*.
 
; (ii): The current expression will hold our identifier which will be the body to evaluate. On *identifier the body will be evaluated in our table and return the evaluation of our
;       procedure based on the previously defined bindings within the table. 

; (iii); The current expression e is an application, (e1, e2). If let* occurs in e it will occur in e1, e2 as our table stores our bindings and procedures as we proceed by building a 
;        new entry. For e when let* occurs our TLS interpreter on my-apply-closure will build a new list entry storing its rib essentially. Which will thus be stored in our table
;        already once our application is called, and as such when we do lookup-in-table the binding will already be found as it was stored prior to the application call.

; (iv): In a list-to-action expression it's condition will be validated in *application. then using my apply we are able to identify procedures as a nonprimative and use my
;       my-apply-closure to evaluate the closure of our procedure using the table.

; (v): If e is a lambda expression, when we step through the TLS interpeter we will identify *lambda in list-to-action. Thus continuing through the interpreter we will create a
;      closure containing the formals, values, and body. If let* occurs it will then be within the body of our closure. Likewise, each proceeding lambda will then be the body
;      of our prior lambda.

;(vi); Given that TLS without let* correctly evaluates the given quoted procedure,the *let* function will evaluate our quoted let procedure. *let* will utilize the meaning function
;      to evaluate our expression by taking the environment for our let*. In the environment for our let* we seperate our formals into their respective list and values associated
;      into their own respective list, returning our let body at the end when our formals and values have been seperated. Thus, once returned to meaning our environment will have
;      been created and meaning will proceed through TLS evaluations normally, evaluating our body (which is a TLS expression) via the entries in the table.


(define *let*
  (lambda (e table)
    (meaning (make-let*-env (cadr e) (let-body e))table)))


(define let-body caddr)

#|
make-let*-env

Pre: the vars to be binded and the body of a let function are given

Pre: nested environments of the let* lambdas are returned

D&C: we will create an environment for each subsequent binding in let*, increasing the lambda nesting depth by one for each
binding. Every newly created lambda expression will have to consist of formals, arguments, and body. The bindings given will contain
the formals and the arg that can be extracted from the bindings. The body of the newly created lambda will be the next lambda created
from the proceeding binding, which again increases the nesting depth. The final binding will hold the body of the let* fucntion
which would allow it to access all of the arguments from the previous closures(bindings)


Basis: the bindings have all been extracted and a lambda closure has been created for each

IH: Assume all of the subsequent bindings have created the appropriate closures and the body of the let* has been accurately placed
as the body of the innermost closure

IS: the return value of the previous function will be placed as the body of the environment of the lambda expression that is currently being created.
This would mean that all subsequent lambda closure can access the variables from the newly created lambda expression. 
|#


(define (make-let*-env form-val body)
  (cond ((null? form-val) body)
        (else (append (list (list (quote lambda)
                (list (extract-formals form-val)) (make-let*-env (cdr form-val) body))) (list (extract-arguments form-val))))))

(define extract-formals caar)

(define extract-arguments cadar)


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
         ((eq? (car e) (quote let*))
          *let*)
         (else *application)))
      (else *application))))
