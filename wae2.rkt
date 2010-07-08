#lang plai

(require rackunit)
(require rackunit/text-ui)
(require rackunit/gui)

#|
Concrete syntax of WAE in BNF
<WAE> ::= | <num>
          | {+ <WAE> <WAE>}
          | {- <WAE> <WAE>}
          | {with {<id> <WAE>} WAE}
          | <id>
|#

;; Abstract syntax of WAE
(define-type WAE
  (num (n number?))
  (add (lhs WAE?) (rhs WAE?))
  (sub (lhs WAE?) (rhs WAE?))
  (neg (expr WAE?))
  (pos (expr WAE?))
  (with (name symbol?) (named-expr WAE?) (body WAE?))
  (id (name symbol?)))

;; Test for WAE
(define-test-suite wae-test-suite
  (check-false (WAE? '()))
  (check-false (WAE? 5))
  (check-true (WAE? (num 5)))
  (check-true (WAE? (add (num 1) (num 2))))
  (check-exn exn? (lambda () (WAE? (add (num 1) 2))))
  (check-true (WAE? (sub (num 5) (num 2))))
  (check-true (WAE? (add (num 5) (sub (num 7) (num 1)))))
  (check-false (WAE? 5))
  (check-exn exn? (lambda () (WAE? (add (num 6) (num 8) (num 9)))))
  (check-exn exn? (lambda () (WAE? (add (num 4)))))
  (check-exn exn? (lambda () (WAE? (add))))
  (check-true (WAE? (id 'x)))
  (check-exn exn? (lambda () (WAE? (id 6))))
  (check-true (WAE? (with 'x (num 5) (id 'x))))
  (check-true (WAE? (with 'x (add (num 5) (num 3)) (id 'x)))))

(run-tests wae-test-suite)

;; parse: sexp -> WAE
;; to convert sexpressions into WAEs
(define (parse sexp)
  (cond
    ((number? sexp) (num sexp))
    ((symbol? sexp) (id sexp))
    ((list? sexp)
     (let ((len (length sexp)))
       (case (first sexp)
         ((+)
          (case len
            ((2) (pos (parse (second sexp))))
            ((3) (add (parse (second sexp)) (parse (third sexp))))
            (else (error "syntax error"))))
         ((-) 
          (case len
            ((2) (neg (parse (second sexp))))
            ((3) (sub (parse (second sexp))
                      (parse (third sexp))))
            (else (error "syntax error"))))
         ((with)          
          (if (and (eq? (length sexp) 3)
                   (eq? (length (second sexp)) 2)
                   (symbol? (first (second sexp))))
              (with (first (second sexp))
                    (parse (second (second sexp)))
                    (parse (third sexp)))
              (error "syntax error")))
         (else (error "syntax error")))))
    (else (error "syntax error"))))

;; Test for parse
(define-test-suite parse-test-suite
  (check-exn exn? (lambda () (parse '())))
  (check-exn exn? (lambda () (parse '(4))))
  (check-equal? (parse '5) (num 5))
  (check-equal? (parse '{+ 2 1}) (add (num 2) (num 1)))
  (check-equal? (parse '{- 6 2}) (sub (num 6) (num 2)))
  (check-equal? (parse '{+ {+ 3 1} 7}) (add (add (num 3) (num 1)) (num 7)))
  (check-equal? (parse '{- {+ 1 6} {- 9 2}}) (sub (add (num 1) (num 6)) (sub (num 9) (num 2))))
  (check-exn exn? (lambda () (parse '(+))))
  (check-exn exn? (lambda () (parse '(-))))
  (check-equal? (parse '{+ 4}) (pos (num 4)))
  (check-equal? (parse '{- 4}) (neg (num 4)))
  (check-exn exn? (lambda () (parse '(+ 1 2 3))))
  (check-equal? (parse '{+ a b}) (add (id 'a) (id 'b)))
  (check-equal? (parse '{+ a 1}) (add (id 'a) (num 1)))
  (check-equal? (parse 'a) (id 'a))
  (check-equal? (parse '{with {x 1} {+ x 2}}) (with 'x (num 1) (add (id 'x) (num 2))))
  (check-exn exn? (lambda () (parse '{with x})))
  (check-exn exn? (lambda () (parse '{with x 2})))
  (check-exn exn? (lambda () (parse '{with})))
  (check-exn exn? (lambda () (parse '{with {5 x} {+ x 1}})))
  (check-exn exn? (lambda () (parse '{with {+ 3 5} {+ 3 1}})))
  (check-equal? (parse '{with {x {+ 4 {- y 5}}} {+ x y}})
                (with 'x (add (num 4) (sub (id 'y) (num 5))) (add (id 'x) (id 'y)))))

(run-tests parse-test-suite)

;; Some definitions:
;;
;; Binding Instance: A binding instance of an identifier is the instance of the
;; identifier that gives it its value. In WAE, the <id> position of a with
;; is the only binding instance.
;;
;; Scope: The scope of a binding instance is the region of program text in
;; which instances of the identifier refer to the value bound by the binding instance.
;;
;; Bound Instance: An identifier is bound if it is contained within the scope 
;; of a binding instance of its name.
;;
;; Free Instance: An identifier not contained in the scope of any binding 
;; instance of its name is said to be free.

;; Step by step Substitution definition

;; calc: AE -> number
;; consumes an AE and computes the corresponding number
(define (calc expr)
  (type-case WAE expr
    (num (n) n)
    (add (lhs rhs) (+ (calc lhs) (calc rhs)))
    (sub (lhs rhs) (- (calc lhs) (calc rhs)))
    (neg (expr) (- (calc expr)))
    (pos (expr) (calc expr))
    (with (sub-id named-expr body)
          (error "not implemented"))
    (id (name) (error "not implemented"))))

(define-test-suite calc-test-suite
  (check-equal? (calc (parse '3)) 3)
  (check-equal? (calc (parse '{+ 3 4})) 7)
  (check-equal? (calc (parse '{+ {- 3 4} 7})) 6)
  (check-equal? (calc (parse '{+ {- 1 0}})) 1))

(run-tests calc-test-suite)
