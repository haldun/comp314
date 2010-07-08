#lang planet plai/plai:1:3 
(require htdp/testing)

#|
Concrete syntax in BNF
AE ::= <number>
     | (+ <AE> <AE>)
     | (- <AE> <AE>)
|#

;; Abstract syntax definition based on our concrete syntax.
(check-expect (AE? 3) #f)
(check-expect (AE? (num 4)) #t)
(check-expect (AE? (add (num 5) (num 3))) #t)
(check-expect (AE? (sub (num 6) (num 1))) #t)
(check-error (AE? (add (num 5))) 
             "procedure add: expects 2 arguments, given 1: '(num 5)")
(check-error (AE? (add (num 1) (num 2) (num 3))) 
             "procedure add: expects 2 arguments, given 3: '(num 1) '(num 2) '(num 3)")

(define-type AE
  (num (n number?))
  (add (lhs AE?) (rhs AE?))
  (sub (lhs AE?) (rhs AE?)))

;; parse: sexp -> AE
;; to convert s-expressions into AEs
(check-expect (parse '5) (num 5))
(check-expect (parse '(+ 5 3)) (add (num 5) (num 3)))
(check-expect (parse '(+ 2 (- 7 3))) (add (num 2) (sub (num 7) (num 3))))
(check-expect (parse '(- 3 5)) (sub (num 3) (num 5)))
(check-error (parse 'a) "Syntax error in a")
(check-error (parse '(+ 5)) "Syntax error in (+ 5)")

(define (parse sexp)
  (cond
    ((number? sexp) (num sexp))
    ((list? sexp)
     (case (first sexp)
       ((+) (add (parse (second sexp))
                 (parse (third sexp))))
       ((-) (sub (parse (second sexp))
                 (parse (third sexp))))))
    (else (error "Syntax error in" sexp))))

;; calc: AE -> number
;; consumes an AE and computes the corresponding number
(check-expect (calc (parse '3)) 3)
(check-expect (calc (parse '(+ 3 4))) 7)
(check-expect (calc (parse '(+ (- 3 4) 7))) 6)

(define (calc ae)
  (type-case AE ae
             (num (n) n)
             (add (left right) (+ (calc left) (calc right)))
             (sub (left right) (- (calc left) (calc right)))))

(define (interp sexp)
  (calc (parse sexp)))

;; Generate test reports
(generate-report)