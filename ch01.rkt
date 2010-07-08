#lang plai

(require rackunit)
(require rackunit/text-ui)
(require rackunit/gui)

#|
Concrete syntax of AE in BNF
<AE> ::= | <num>
         | (+ <AE> <AE>)
         | (- <AE> <AE>)
|#

;; Abstract syntax of AE
(define-type AE
  (num (n number?))
  (add (lhs AE?) (rhs AE?))
  (sub (lhs AE?) (rhs AE?))
  (neg (expr AE?)))

;; Test for AE
(define-test-suite ae-test-suite
 (check-false (AE? '()))
 (check-false (AE? 5))
 (check-true (AE? (num 5)))
 (check-true (AE? (add (num 1) (num 2))))
 (check-exn exn? (lambda () (AE? (add (num 1) 2))))
 (check-true (AE? (sub (num 5) (num 2))))
 (check-true (AE? (add (num 5) (sub (num 7) (num 1)))))
 (check-false (AE? 5))
 (check-exn exn? (lambda () (AE? (add (num 6) (num 8) (num 9)))))
 (check-exn exn? (lambda () (AE? (add (num 4)))))
 (check-exn exn? (lambda () (AE? (add)))))

(run-tests ae-test-suite)

;; parse: sexp -> AE
;; to convert sexpressions into AEs
(define (parse sexp)
  (cond    
    ((number? sexp) (num sexp))
    ((list? sexp)     
     (case (first sexp)
       ((+) 
        (case (length sexp)
          ((2) (parse (second sexp)))
          ((3) (add (parse (second sexp))
                    (parse (third sexp))))
          (else (error "syntax error"))))
        ((-) 
         (case (length sexp)
           ((2) (neg (parse (second sexp))))
           ((3) (sub (parse (second sexp))
                     (parse (third sexp))))
           (else (error "syntax error"))))
       (else (error "syntax error"))))
    (else (error "syntax error"))))

;; Test for parse
(define-test-suite parse-test-suite
  (check-exn exn? (lambda () (parse '())))
  (check-exn exn? (lambda () (parse '(4))))
  (check-equal? (parse '5) (num 5))
  (check-equal? (parse '(+ 2 1)) (add (num 2) (num 1)))
  (check-equal? (parse '(- 6 2)) (sub (num 6) (num 2)))
  (check-equal? (parse '(+ (+ 3 1) 7)) (add (add (num 3) (num 1)) (num 7)))
  (check-equal? (parse '(- (+ 1 6) (- 9 2))) (sub (add (num 1) (num 6)) (sub (num 9) (num 2))))
  (check-exn exn? (lambda () (parse '(+))))
  (check-exn exn? (lambda () (parse '(+ -))))
  (check-equal? (parse '(+ 4)) (num 4))
  (check-equal? (parse '(- 4)) (neg (num 4)))
  (check-exn exn? (lambda () (parse '(+ 1 2 3))))
  (check-exn exn? (lambda () (parse '(+ a b))))
  (check-exn exn? (lambda () (parse '(+ a 1))))
  (check-exn exn? (lambda () (parse 'a))))

(run-tests parse-test-suite)

;; calc: AE -> number
;; consumes an AE and computes the corresponding number
(define (calc expr)
  (type-case AE expr
    (num (n) n)
    (add (lhs rhs) (+ (calc lhs) (calc rhs)))
    (sub (lhs rhs) (- (calc lhs) (calc rhs)))
    (neg (expr) (- (calc expr)))))

(define-test-suite calc-test-suite
  (check-equal? (calc (parse '3)) 3)
  (check-equal? (calc (parse '{+ 3 4})) 7)
  (check-equal? (calc (parse '{+ {- 3 4} 7})) 6))

(run-tests calc-test-suite)
