#lang plai-typed

;(require "tests.rkt")

;; starter file for the extended basic interpreter assignment

;---------------------------------------------------------------------------------
;; surface syntax and parser : you should NOT need to edit this section

; type used to capture a with-binding
(define-type DefS
  [defS (name : symbol) (val : ExprS)])

; surface syntax for expressions
(define-type ExprS
  [numS (n : number)]
  [plusS (l : ExprS) (r : ExprS)]
  [bminusS (l : ExprS) (r : ExprS)]
  [multS (l : ExprS) (r : ExprS)]
  [idS (i : symbol)]
  [appS (f : ExprS) (args : (listof ExprS))]
  [if0S (c : ExprS) (t : ExprS) (e : ExprS)]
  [funS (params : (listof symbol)) (body : ExprS)]
  [withS (bindings : (listof DefS)) (body : ExprS)])

; parses s-expressions into surface syntax
(define (parse [s : s-expression]) : ExprS
  (cond
    [(s-exp-number? s) (numS (s-exp->number s))]
    [(s-exp-symbol? s) (idS (s-exp->symbol s))]
    [(s-exp-list? s)
     (let ([sl (s-exp->list s)])
       (cond [(s-exp-symbol? (first sl)) ;; built-in construct or calling function through an identifier
              (case (s-exp->symbol (first sl))
                [(+) (plusS (parse (second sl)) (parse (third sl)))]
                [(*) (multS (parse (second sl)) (parse (third sl)))]
                [(-) (bminusS (parse (second sl)) (parse (third sl)))]
                [(if0) (if0S (parse (second sl)) (parse (third sl)) (parse (fourth sl)))]
                [(fun) (funS (map s-exp->symbol (s-exp->list (second sl))) 
                             (parse (third sl)))]
                [(with) (withS (map (lambda (b) 
                                      (let ([bl (s-exp->list b)])
                                        (defS (s-exp->symbol (first bl)) (parse (second bl)))))
                                    (s-exp->list (second sl)))
                               (parse (third sl)))]
                [else ;; must be a function call using function name
                 (appS (idS (s-exp->symbol (first sl)))
                       (map parse (rest sl)))])]
             [(s-exp-list? (first sl)) ;; function call with complex expression in function position
              (appS (parse (first sl))
                    (map parse (rest sl)))]
             ;; March 26, 2016 -- the following two lines are new
             [(s-exp-number? (first sl)) ;; type violation of using number as function but fits grammar
              (appS (parse (first sl)) (map parse (rest sl)))]
             ;[else (error 'parse "expected symbol or list after parenthesis")]))]
             [else (error 'parse "grammar violation: expected symbol or list after parenthesis")]))]
    ;[else (error 'parse "unexpected input format")]))
    [else (error 'parse "grammar violation: unexpected input format")]))


;---------------------------------------------------------------------------------
#|
; surface syntax for expressions
(define-type ExprS
  [numS (n : number)]
  [plusS (l : ExprS) (r : ExprS)]
  [bminusS (l : ExprS) (r : ExprS)]
  [multS (l : ExprS) (r : ExprS)]
  [idS (i : symbol)]
  [appS (f : ExprS) (args : (listof ExprS))]
  [if0S (c : ExprS) (t : ExprS) (e : ExprS)]
  [funS (params : (listof symbol)) (body : ExprS)]
  [withS (bindings : (listof DefS)) (body : ExprS)])
|#

;; abstract syntax and desugar


(define-type ExprC
  [numC (n : number)]
  [plusC (l : ExprC) (r : ExprC)]
  [multC (l : ExprC) (r : ExprC)]
  [idC (i : symbol)]
  [appC (f : ExprC) (args : (listof ExprC))]
  [if0C (c : ExprC) (t : ExprC) (e : ExprC)]
  [funC (params : (listof symbol)) (body : ExprC)])

;; desugar -- returning a default/dummy value so file can be run
(define (desugar [e : ExprS]) : ExprC
  (type-case ExprS e
    [numS (n) (numC n)]
    [plusS (l r) (plusC (desugar l)
                        (desugar r))]
    [bminusS (l r) (plusC (desugar l)
                          (multC (numC -1) (desugar r)))]
    [multS (l r) (multC (desugar l)
                        (desugar r))]
    [idS (i) (idC i)]
    [appS (f a) (appC (desugar f) (desugar-list a))]
    [if0S (c t e) (if0C (desugar c) (desugar t) (desugar e))]
    [funS (p b) (funC p (desugar b))]
    [withS (bindings body) (appC (funC (map (λ (defs)
                                              (defS-name defs))
                                            bindings)
                                       (desugar body))
                                 (map (λ (defs)
                                        (desugar (defS-val defs))) 
                                        bindings))]))

;; desugar-list:
(define (desugar-list [args : (listof ExprS)]) : (listof ExprC)
  (cond
    [(empty? args) empty]
    [(cons? args) (cons (desugar (first args)) (desugar-list (rest args)))]))

;---------------------------------------------------------------------------------
;; output values

(define-type Value
  [numV (n : number)]
  [closV (arg : (listof symbol)) (body : ExprC) (env : Env)])

;---------------------------------------------------------------------------------
;; Environments

;; binding an identifier to a value
(define-type Binding
  [bind (name : symbol) (val : Value)])

(define-type-alias Env (listof Binding))
(define extend-env cons)
(define mt-env empty)

; define num+
; num+ : Value Value -> Value
(define (num+ [l : Value] [r : Value]) : Value
  (cond
    [(and (numV? l) (numV? r))
     (numV (+ (numV-n l) (numV-n r)))]
    [else
     (error 'num+ "type error: one argument was not a number")]))

; define num*
; num* : Value Value -> Value
(define (num* [l : Value] [r : Value]) : Value
  (cond
    [(and (numV? l) (numV? r))
     (numV (* (numV-n l) (numV-n r)))]
    [else
     (error 'num* "type error: one argument was not a number")]))

; lookup: symbol Env -> Value
; look for the value of a symbol in an Env
(define (lookup [for : symbol] [env : Env]) : Value
  (cond
    [(empty? env) (error 'lookup "unbound error: name not found in Env")]
    [else (cond
            [(symbol=? for (bind-name (first env)))
             (bind-val (first env))]
            [else (lookup for (rest env))])]))

;[else (numV 0)])]))

;---------------------------------------------------------------------------------
#|
(define-type ExprC
  [numC (n : number)]
  [plusC (l : ExprC) (r : ExprC)]
  [multC (l : ExprC) (r : ExprC)]
  [idC (i : symbol)]
  [appC (f : ExprC) (arg : ExprC)]
  [if0C (c : ExprC) (t : ExprC) (e : ExprC)]
  [funC (param : symbol) (body : ExprC)])
|#

;; interp 
(define (interp [e : ExprC] [env : Env]) : Value
  (type-case ExprC e
    [numC (n) (numV n)]
    [plusC (l r) (num+ (interp l env) (interp r env))]
    [multC (l r) (num* (interp l env) (interp r env))]
    [idC (i) (lookup i env)]
    [appC (fun arg) (type-case Value (interp fun env)
                      [numV (n) (error 'interp "type error: can't use appC on a number")]
                      [closV (n body c-env)
                             (interp body (append (extra-env env n arg)
                                                  c-env))])]            
    [if0C (c t e) (type-case Value (interp c env)
                    [numV (n) (if (zero? n)
                                  (interp t env)
                                  (interp e env))]
                    [else (error 'interp "type error: condition can't be interpreted into a number")])]
    [funC (p b) (cond
                  [(duplicate-arg p) (error 'interp "multiple")]
                  [else (closV p b env)])]))

; duplicate-arg
(define (duplicate-arg [p-list : (listof symbol)]) : boolean
  [cond
    [(empty? p-list) false]
    [(cons? p-list)
     (if (member (first p-list) (rest p-list))
         true 
         (duplicate-arg (rest p-list)))]])
          

; extra-env: 
(define (extra-env [env : Env] [args : (listof symbol)] [body : (listof ExprC)]) : Env
  (cond
    [(empty? args) empty]
    [(cons? args)
     (let ([val (interp (first body) env)])
       (extend-env (bind (first args) val)
                   (extra-env env (rest args) (rest body))))]))


;---------------------------------------------------------------------------------
;; API for running programs

; evaluates a program starting with a pre-populated environment
; (this can be helpful in testing)
(define (run/env sexp env)
  (interp (desugar (parse sexp)) env))

; evaluates a program in the empty environment
(define (run sexp)
  (run/env sexp mt-env))

(test (run '(with ([a 3])
                  (with ([c 1])
                        (with ([a 2])
                              (+ a c)))))
      (numV 3))



