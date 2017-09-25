#lang plai-typed

;; starter file for the testing mutation assignment

; You are NOT EXPECTED TO EDIT THIS FILE.  This file gives you a stub for
; running your programs with the same types as our autograding script expects.
; You are not expected to fill in the interpreter, desugar, etc for this assignment.

(require (typed-in racket/sandbox [call-with-limits : (number boolean (-> 'a) -> 'a)]))

;---------------------------------------------------------------------------------
;; surface syntax and parser 

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
  [lamS (params : (listof symbol)) (body : ExprS)]
  [withS (bindings : (listof DefS)) (body : ExprS)]
  [boxS (arg : ExprS)]
  [unboxS (b : ExprS)]
  [setboxS (b : ExprS) (v : ExprS)]
  [seqS (e1 : ExprS) (e2 : ExprS)]
  [setS (var : symbol) (val : ExprS)]
  )

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
                [(fun) (lamS (map s-exp->symbol (s-exp->list (second sl))) 
                             (parse (third sl)))]
                [(with) (withS (map (lambda (b) 
                                      (let ([bl (s-exp->list b)])
                                        (defS (s-exp->symbol (first bl)) (parse (second bl)))))
                                    (s-exp->list (second sl)))
                               (parse (third sl)))]
                [(box) (boxS (parse (second sl)))]
                [(unbox) (unboxS (parse (second sl)))]
                [(setbox) (setboxS (parse (second sl)) (parse (third sl)))]
                [(seq) (seqS (parse (second sl)) (parse (third sl)))]
                [(set) (setS (s-exp->symbol (second sl)) (parse (third sl)))]
                [else ;; must be a function call using function name
                 (appS (idS (s-exp->symbol (first sl)))
                       (map parse (rest sl)))])]
             [(s-exp-list? (first sl)) ;; function call with complex expression in function position
              (appS (parse (first sl))
                    (map parse (rest sl)))]
             [(s-exp-number? (first sl)) ;; type violation of using number as function but fits grammar
              (appS (parse (first sl)) (map parse (rest sl)))]
             [else (error 'parse "expected symbol or list after parenthesis")]))]
    [else (error 'parse "unexpected input format")]))

;---------------------------------------------------------------------------------
;; ExprC

(define-type ExprC
  [numC (n : number)]
  [plusC (l : ExprC) (r : ExprC)]
  [multC (l : ExprC) (r : ExprC)]
  [idC (i : symbol)]
  [appC (f : ExprC) (args : (listof ExprC))]
  [if0C (c : ExprC) (t : ExprC) (e : ExprC)]
  [lamC (params : (listof symbol)) (body : ExprC)]
  [boxC (arg : ExprC)]
  [unboxC (b : ExprC)]
  [setboxC (b : ExprC) (val : ExprC)]
  [seqC (e1 : ExprC) (e2 : ExprC)]
  [setC (var : symbol) (val : ExprC)]
  )

;---------------------------------------------------------------------------------
;; Environments

(define-type Binding
  [bind (name : symbol) (loc : Location)])
(define-type-alias Env (listof Binding))
(define-type-alias Location number)

;---------------------------------------------------------------------------------
;; output values

(define-type Value                                                                                                                             
  [numV (n : number)]                                                                                                                          
  [boxV (l : Location)]                                                                                                                        
  [closV (params : (listof symbol)) (body : ExprC) (env : Env)])   

;---------------------------------------------------------------------------------
;; API for running programs

; running functions with timeout to trap infinite loops
(define (run [sexp : s-expression]) : Value
  (let ([p (parse sexp)])
    (call-with-limits 
     10 #f
     (lambda () (numV 0)))))


