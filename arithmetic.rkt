#lang plai

(require (lib "trace.ss"))

;; Concrete syntax of the language
;; <AE> ::= <num>
;;        | {+ <AE> <AE>}
;;        | {with {<id> <AE>} <AE>}
;;        | <id>
;;        | {<id> <AE>}

;; Abstract syntax of the language
(define-type AE
  (num (n number?))
  (add (lhs AE?) (rhs AE?))
  (with (name symbol?) (named-expr AE?) (body AE?))
  (id (name symbol?))
  (app (fun-name symbol?) (arg AE?)))

(define-type FunDef
  (fundef (fun-name symbol?)
          (arg-name symbol?)
          (body AE?)))

(define-type DeferredSub
  (emptySub)
  (sub (name symbol?) (value number?) (ds DeferredSub?)))

;; parse: sexp -> AE
;; to convert sexpressions to AEs
(define (parse sexp)
  (cond
    ((number? sexp) (num sexp))
    ((symbol? sexp) (id sexp))
    ((and (list? sexp) (not (null? sexp)))
     (let ((sexp-length (length sexp)))
       (case (first sexp)
         ((+) 
          (if (eq? sexp-length 3)
              (add (parse (second sexp))
                   (parse (third sexp)))
              (error "parse error")))
         ((with)
          ;; check syntax of the 'with' expression
          ;; with takes exactly 3 arguments
          ;; second argument must be a 2-element list
          ;; first element of the second argument is the name of identifier
          ;; so it must be a symbol.
          ;; FIXME: Some values can be cached in this function
          (if (and (eq? sexp-length 3)
                   (list? (second sexp))
                   (eq? (length (second sexp)) 2)  
                   (symbol? (first (second sexp))))
              (with (first (second sexp))
                    (parse (second (second sexp)))
                    (parse (third sexp)))
              (error "parse error")))
         (else
          ;; Is expr is a function application?
          (if (and (eq? sexp-length 2) (symbol? (first sexp)))
              (app (first sexp) (parse (second sexp)))
              (error "parse error"))))))                     
    (else (error "parse error"))))

;; FIXME Add more test cases
(test (parse '65) (num 65))
(test (parse '{+ 4 2}) (add (num 4) (num 2)))
(test (parse '{+ {+ 1 4} {+ 5 {+ 2 3}}}) 
             (add (add (num 1) (num 4)) (add (num 5) (add (num 2) (num 3)))))
(test (parse '{with {x 5} x}) 
      (with 'x (num 5) (id 'x)))
(test (parse '{with {x {+ 4 y}} {+ x x}})
      (with 'x (add (num 4) (id 'y)) (add (id 'x) (id 'x))))
(test (parse '{with {x y} {+ x {+ 3 y}}})
      (with 'x (id 'y) (add (id 'x) (add (num 3) (id 'y)))))
(test/exn (parse '{}) "parse error")
(test/exn (parse '{4}) "parse error")
(test/exn (parse '{4 + 4}) "parse error")
(test/exn (parse '{- 4 2}) "parse error")
(test/exn (parse '{+ 4 1 3}) "parse error")
(test/exn (parse '{a}) "parse error")
(test/exn (parse '{with 5 3 4}) "parse error")
(test/exn (parse '{with {4 2} 5}) "parse error")
(test/exn (parse '{with {x 1 2} {+ x 1}}) "parse error")
(test (parse '{f 4}) (app 'f (num 4)))
(test (parse '{with {x 5} {f x}}) 
      (with 'x (num 5) (app 'f (id 'x))))
(test (parse '{with {x y}
                {with {y z}
                  {+ {f {g z}} {+ x y}}}})
      (with 'x (id 'y)
            (with 'y (id 'z)
                  (add (app 'f (app 'g (id 'z)))
                       (add (id 'x) (id 'y))))))
(test/exn (parse '{5 x}) "parse error")
(test/exn (parse '{x y z}) "parse error")
(test/exn (parse '{x}) "parse error")

;; subst: AE symbol AE -> AE
;; substitutes second argument with third argument in first argument
(define (subst expr sub-id val)
  (type-case AE expr
    (num (n) expr)
    (add (left right) (add (subst left sub-id val)
                           (subst right sub-id val)))
    (with (bound-id named-expr bound-body)
          (if (symbol=? bound-id sub-id)
              (with bound-id
                    (subst named-expr sub-id val)
                    bound-body)
              (with bound-id 
                    (subst named-expr sub-id val)
                    (subst bound-body sub-id val))))
    (id (v) (if (symbol=? v sub-id) val expr))
    (app (fun-name arg) (app fun-name (subst arg sub-id val)))))

;; (trace subst)
;; FIXME Add test cases for subst


;; lookup-fundef: fun-name fun-defs -> fun-def
(define (lookup-fundef fun-name fun-defs)
  (if (empty? fun-defs)
      (error fun-name "function not found")
      (if (symbol=? fun-name (fundef-fun-name (first fun-defs)))
          (first fun-defs)
          (lookup-fundef fun-name (rest fun-defs)))))

(test (lookup-fundef 'f
        (list (fundef 'f 'x (id 'x))))
      (fundef 'f 'x (id 'x)))

;; lookup: symbol DeferredSub -> AE
(define (lookup name ds)
  (type-case DeferredSub ds
    (emptySub () (error 'lookup "no binding for identifier"))
    (sub (bound-name bound-value rest-ds)
         (if (symbol=? bound-name name)
             bound-value
             (lookup name rest-ds)))))

;; interp: AE listof(fundef) deferredsub -> number
;; consumes an AE and computes the correspoding number
(define (interp ae fun-defs ds)
  (type-case AE ae
    (num (n) n)
    (add (lhs rhs) (+ (interp lhs fun-defs ds) (interp rhs fun-defs ds)))
    (with (bound-id named-expr bound-body)
          (interp bound-body
                  fun-defs
                  (sub bound-id (interp named-expr fun-defs ds) ds)))    
    (id (v) (lookup v ds))
    (app (fun-name arg-expr)
         (local ((define the-fun-def (lookup-fundef fun-name fun-defs)))
           (interp (fundef-body the-fun-def)
                   fun-defs
                   (sub (fundef-arg-name the-fun-def)
                        (interp arg-expr fun-defs ds)
                        ds))))))
;(trace interp)

(test (interp (parse 4) '() (emptySub)) 4)
(test (interp (parse '{+ 1 5}) '() (emptySub)) 6)
(test (interp (parse '{+ {+ 6 2} {+ 9 1}}) '() (emptySub)) 18)
(test (interp (parse '{with {x {+ 5 5}} {+ x x}}) '() (emptySub)) 20)
(test (interp (parse '5) '() (emptySub)) 5)
(test (interp (parse '{+ 5 5}) '() (emptySub)) 10)
(test (interp (parse '{with {x {+ 5 5}} {+ x x}}) '() (emptySub)) 20)
(test (interp (parse '{with {x 5} {+ x x}}) '() (emptySub)) 10)
(test (interp (parse '{with {x 5} {+ x {with {x 3} 10}}}) '() (emptySub)) 15)
(test (interp (parse '{with {x 5} {+ x {with {x 3} x}}}) '() (emptySub)) 8)
(test (interp (parse '{with {x 5} {+ x {with {y 3} x}}}) '() (emptySub)) 10)
(test (interp (parse '{with {x 5} {with {y x} y}}) '() (emptySub)) 5)
(test (interp (parse '{with {x 5} {with {x x} x}}) '() (emptySub)) 5)
(test (interp (parse '{f 5})
              (list (fundef 'f 'x (id 'x)))
              (emptySub)) 5)
(test (interp (parse '{f 3})
              (list (fundef 'f 'x (add (id 'x) (id 'x))))
              (emptySub)) 6)
(test (interp (parse '{f {f 3}})
              (list (fundef 'f 'x (add (id 'x) (id 'x))))
              (emptySub)) 12)
(test (interp (parse '{f {+ 4 {f 7}}})
              (list (fundef 'f 'x (add (id 'x) (id 'x))))
              (emptySub)) 36)
(test (interp (parse '{with {x 5} {f x}})
              (list (fundef 'f 'x (add (id 'x) (num 10))))
              (emptySub)) 15)
(test (interp (parse '{+ x x}) '() (sub 'x 4 (emptySub))) 8)
(test (interp (parse '{+ x {with {y {+ x x}} {+ y 10}}})
              '()
              (sub 'x 5 (emptySub))) 25)
(test (interp (parse '{f x})
              (list (fundef 'f 'a (add (id 'a) (num 7))))
              (sub 'x 11 (emptySub)))
      18)
(test (interp (parse '{f f})
              (list (fundef 'f 'a (add (id 'a) (num 10))))
              (sub 'f 10 (emptySub)))
      20)