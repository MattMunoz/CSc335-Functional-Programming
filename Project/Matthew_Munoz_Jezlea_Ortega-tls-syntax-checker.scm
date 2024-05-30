; CSc 335, Spring 2024 Project
;;; Name: Matthew Munoz, Jezlea Ortega

;;; Spring 2024 Project Question #2 (TLS extended syntax-checker)

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

(define extend-table cons)



(define lookup-in-entry
  (lambda (name entry entry-f)
    (lookup-in-entry-help name
                          (names entry)
                          entry-f)))

(define new-entry build)

(define names
  (lambda (entry) (car entry)))

(define vals
  (lambda (entry) (cadr entry)))

;Not used with our altered syntax-checker code
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

;Not used with our altered syntax-checker code
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

;Not used with our altered syntax-checker code
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
  (lambda (e table)
    (cond 
      ((number? e) e)
      ((eq? e #t) #t)
      ((eq? e #f) #f)
      (else (build (quote primitive) e)))))

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


(define else?
  (lambda (x)
    (cond 
      ((atom? x) (eq? x (quote else)))
      (else #f))))

(define question-of first)

(define answer-of second)

(define cond-lines-of cdr)

(define evlis
  (lambda (args table)
    (cond 
      ((null? args) (quote ()))
      (else
       (cons (meaning (car args) table)
             (evlis (cdr args) table))))))

(define function-of car)

(define arguments-of cdr)

(define primitive?
  (lambda (l)
    (eq? (first l) (quote primitive))))

(define non-primitive?
  (lambda (l)
    (eq? (first l) (quote non-primitive))))

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
#|
PROOF: by structural induction on syntax checker

Basis: number, booleans, primatives all belong to TLS so syntax checker correctly evaluates them 

IH: all individual elements provided to the expression have been properly identified and validated within TLS via the check fucntion

IS:

(i) the given expression or atom e is properly identified as existing within check, which by extension validates it as an  existing primative or non-primative
     - returns true if its a valid existing expression in TLS-syntax-checker
     - otherwise returns a message identifying the erronous primitive/nonprimitive

(ii) assuming the expression gets accurately identified as being a part of TLS and by extension a primative or non-primative, we can then proceed with TLS expression-to-action. The expression will be correctly evaluated as an identifier or application, since we can assume that TLS corretly identifies an application and identifier we can assume syntax checker also correctly evaluates it as an identifier or application

(iiia) if the expression given is an identifer we can assume TLS will acurately look in that scopes table. However, the scope will only include the variables that are to be identified, not the arguments. So, a recursive look into the table at each level of the accessible environment will identify it as either being bound or unbound.

(iiib) if the expression is given as an application we know that the given expression has been identified as valid, therefore a check of the given arguments can be made to validate it/them as proper inputs of the given expression. This is beacuse we know that the given arguments within the application must be enclosed within the given expression.

(iv) assuming the application expression has been identified as a proper primative or non primative within TLS,
     - a primative is fully enclosed within its own expression and a simple check on the cdr of the expression is enough to identify if the given values matches the values TLS can accept
     - a non-primative is identified and a closure is created (i.e. environemtn for the formals of the expression)

(v) appication closure creates the environment for the expression and the variable are then placed within the environments scope to allow for proper lookup. We can assume this get handled correctly via the extend-table function in base TLS

(vi) a meaning of the each of the expressions elements is done. The validity of the closures body is done via (i)

(vii) an identification of each of the expressions variables is provided. We can assume that all of the formals of the given expression have been appropriately added to the lookup-table in the respective environemnt. Therefore a recursive identificaiotn of each of the arguments will accurately identify them as being valid(i.e. bound to a valid argument)
    - if all of the variables have been identified as being bound, we can assume that the overall given expression meets all of the criteria for being a valid TLS expression
    - if any of the variables are identified as not being valid, we assume the syntax checker will accruately identify it as a free variable that cannot be properly expresed in TLS and an appropriate error message is dispplayed, naming the unvound variable as such
|#

(define syntax-checker
  (lambda (e)
    (check e (quote () ))))

(define check
  (lambda (e table)
    (cond ((not (valid? e))
           (valid-message e))
          (else ((expression-to-action e) e table)))))

(define valid?
  (lambda (e)
    (cond 
      ((atom? e) (valid-prim? e))
      (else (valid-nonprim? e)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
#|
Changes to atom-to-action and list-to-action, renamed to valid-prim? and valid-nonprim?, respectively

Pre: e, an expression to be evaluated

Post: returns #t if the e contatins a valid, interpretable, primative or nonprimateve within TLS interpreter
|#
(define valid-prim?
  (lambda (e)
    (cond
      ((number? e) #t)
      ((eq? e #t) #t)
      ((eq? e #f) #t)
      ((eq? e (quote cons)) #t)
      ((eq? e (quote car)) #t)
      ((eq? e (quote cdr)) #t)
      ((eq? e (quote null?)) #t)
      ((eq? e (quote eq?)) #t)
      ((eq? e (quote atom?)) #t)
      ((eq? e (quote zero?)) #t)
      ((eq? e (quote add1)) #t)
      ((eq? e (quote mul)) #t)
      ((eq? e (quote sub1)) #t)
      ((eq? e (quote number?)) #t)
      (else #f))))

(define valid-nonprim?
  (lambda (e)
    (cond
      ((atom? (car e))
       (cond 
         ((eq? (car e) (quote quote))
          #t)
         ((eq? (car e) (quote lambda))
          #t)
         ((eq? (car e) (quote cond))
          #t)
         (else (valid-prim? (function-of e)))))
      (else *application))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
#|
Changes to myapply

Pre: a function that has been identified to exist in TLS via the check procedure at the function near the top level and vals that can include arguments or variables

Post: returns #t if the number of arguments matches the number of arguments needed for the given function, otherwise returns appropriate error 
|#
(define myapply
  (lambda (fun vals)
    (cond ((primitive? fun)
           (cond ((arity-prim (second fun) vals) )
                 (else (arity-message-prim (second fun) (length vals)))))
          ((non-primitive? fun)
           (myapply-closure
            (second fun) vals)))))


;;; arity check was implemented on the on the basis on what is properly evaluated by TLS
;;;;Ex. (mul 3 2 56 9) does not throw an error in TLS but only evaluates the firt two inputs(i.e 3 2), therefore an arity of 2 was impleted for mul
(define arity-prim
  (lambda (name vals)
    (cond
      ((eq? name (quote cons))
       (= (length vals) 2)) 
      ((eq? name (quote car))
       (and (= (length vals) 1) (pair? vals) (not (eq? (car vals) '())))) 
      ((eq? name (quote cdr))
       (and (= (length vals) 1) (pair? vals) (not (eq? (car vals) '())))) 
      ((eq? name (quote null?))
       (and (= (length vals) 1) (pair? vals))) 
      ((eq? name (quote eq?))
       (= (length vals) 2))  
      ((eq? name (quote atom?)) 
       (= (length vals) 1)) 
      ((eq? name (quote zero?))
       (= (length vals) 1)) 
      ((eq? name (quote add1))
       (= (length vals) 1)) 
      ((eq? name (quote mul))
       (= (length vals) 2)) 
      ((eq? name (quote sub1))
       (= (length vals) 1)) 
      ((eq? name (quote number?))
       (= (length vals) 1)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
#|
Changes to *quote

Pre: e, the cdr of quote and a table which is the environment for the given cond structure

Post: returns #t if quote is of proper TLS structure, otherwise returns an error messsage if e is of a length which is not equal to 1
|#
(define *quote
  (lambda (e table)
    (cond ((= (length (cdr e)) 1) (text-of e))
                 (else (display "quote: bad syntax, too many values")))))


(define text-of second)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
#|
Changes to *cond

Pre: e, the cdr of the cond structure and a table which is the environment for the given cond structure

Post: returns #t if cond is of proper TLS structure, otherwise returns an error messsage if the cond does not include an else statement or a question/answer pair
|#

(define *application
  (lambda (e table)
    (let ((unbound? (evlis (arguments-of e) table)))
    (cond ((not (valid? (function-of e))) (valid-message e))
          ((member? #f unbound?) (bound-message unbound? (cdr e)))
          (else (myapply
                 (meaning (function-of e) table)
                 unbound?))))))

(define (member? a inlat)
  (cond ((null? inlat) #f)
        (else (or (eq? a (car inlat))
                       (member? a (cdr inlat))))))

(define *cond 
  (lambda (e table)
    (check-line (cond-lines-of e))))

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

(define check-line
  (lambda (e)
    (cond ((null? e) #t)
          ((else? (question-of (car e))) (boolean? (syntax-checker (answer-of (car e)))) )
          ((not (= (length (car e)) 2)) (display "not a question answer pair"))
          ((and (boolean?
                 (syntax-checker (question-of (car e))))
                (boolean? (syntax-checker (answer-of (car e))))) (check-line (cdr e)))
          (else #f))))

(define unbound-value
  (lambda (bound-values table)
    (cond ((eq? #f (car bound-values)) (car table))
          (else (unbound-value (cdr bound-values) (cdr table))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
#|
Changes to the environment 

Pre: a naem(variable) and table is given to the lookup functino

Post: a list of #t and #f stating if the given name is in the environment table
|#

(define myapply-closure
  (lambda (closure vals)
    (cond ((not (eq? (length (formals-of closure)) (length vals))) (lambda-arity-message (length (formals-of closure)) (length vals)))
          (else (meaning (body-of closure)
                         (extend-table
                          (list (formals-of closure))
                          (table-of closure)))))))

(define lookup-in-table
  (lambda (name table table-f)
    (cond 
      ((null? table) #f)
      (else (lookup-in-entry name
                             (car table)
                             (lambda (name)
                               (lookup-in-table name
                                                (cdr table)
                                                table-f)))))))

(define lookup-in-entry-help
  (lambda (name names entry-f)
    (cond
      ((null? names) (entry-f name))
      ((eq? (car names) name) #t)
      (else (lookup-in-entry-help name
                                  (cdr names)
                                  entry-f)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
#|
Messages for erroneous syntax input

Pre: any input given to syntax checker

Post: appropriate message for what caused the syntax error is printed, be it invalid procedure, arity mismatch, or unbounded variables
|#
(define amount-needed
  (lambda (name)
    (cond
      ((eq? name (quote cons))
       "2") 
      ((eq? name (quote car))
       "a list with only at least one element") 
      ((eq? name (quote cdr))
       "a list with only at least one element") 
      ((eq? name (quote null?))
       "1") 
      ((eq? name (quote eq?))
       "2")  
      ((eq? name (quote atom?)) 
       "1") 
      ((eq? name (quote zero?))
      "1") 
      ((eq? name (quote add1))
       "1") 
      ((eq? name (quote mul))
       "2") 
      ((eq? name (quote sub1))
       "1") 
      ((eq? name (quote number?))
       "1"))))

(define valid-message
  (lambda (e)
    (display (car e))
    (display ": undefined")
    (newline)))

(define bound-message
  (lambda (bound-values table)
    (display (unbound-value bound-values table))
    (display " is unbound")))

(define arity-message-prim
  (lambda (func vals)
    (display func)
    (display ":arity mismatch;")
    (newline)
    (display "expected: ")
    (display (amount-needed func))
    (newline)
    (display "given: ")
    (display vals)
    (newline)))

(define lambda-arity-message
  (lambda (a b)
    (display "arity mismatch;")
           (newline)
           (display "expected: ")
           (display a)
           (newline)
           (display "given: ")
           (display b)
           (newline)))

(define arity-message-non-prim            
  (lambda (func variable)
    (display "unbound variable: gave ")
    (display func)
    (display " but variable")
    (display variable)
    (display "has not been bound")))