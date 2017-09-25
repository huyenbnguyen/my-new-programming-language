#lang plai-typed

(require (typed-in racket/sandbox [call-with-limits : (number boolean (-> 'a) -> 'a)]))

;; starter file for objects implementation assignment 

;;;;;;;;;; YOUR EDITS GO IN HERE ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;; Desugaring ;;;;;;

; edit here to desugar class definitions
(define (desugar-class [c : ClassS]) : ExprC
  (lamC (classS-constructor-vars c)         
        (desugar (withS
                  (append (if (symbol=? (classS-parent c) 'Object)
                              empty
                              (list (defS 'parent (newS (classS-parent c)
                                                        (classS-parent-constr-args c)))))
                          (append (classS-private-vars c)
                                  (classS-public-vars c)))                  
                  (lamS (list 'msg)    
                        (varcaseS 'msg
                                  (append (make-getters (classS-public-vars c))
                                          (append (make-setters (classS-public-vars c))
                                                  (defs-methods (classS-methods c))))
                                  (appS (idS 'parent) (list (idS 'msg))))
                        )))))

; id-constructor-vars: (listof symbol) -> (listof ExprS)
; builds a list of idS based on the given list of symbols
(define (id-constructor-vars [los : (listof symbol)]) : (listof ExprS)
  (cond
    [(empty? los) empty]
    [else
     (cons (idS (first los))
           (id-constructor-vars (rest los)))]))

; make-getters: (listof DefS) -> (listof DefS)
; builds a list of DefS for getters
(define (make-getters [lod : (listof DefS)]) : (listof DefS)
  (cond
    [(empty? lod) empty]
    [else
     (cons (defS (string->symbol (string-append "get-" (symbol->string (defS-name (first lod)))))
             (lamS empty (idS (defS-name (first lod)))))
           (make-getters (rest lod)))]))

; make-getters: (listof DefS) -> (listof DefS)
; builds a list of DefS for setters
(define (make-setters [lod : (listof DefS)]) : (listof DefS)
  (cond
    [(empty? lod) empty]
    [else
     (cons (defS (string->symbol (string-append "set-" (symbol->string (defS-name (first lod)))))             
             (lamS (cons 'x empty)
                   (setS (defS-name (first lod))
                         (idS 'x))))
           (make-getters (rest lod)))]))

; make-getters: (listof DefS) -> (listof DefS)
; builds a list of DefS for methods inside the class
(define (defs-methods [m : (listof DefS)]) : (listof DefS)
  (cond
    [(empty? m) empty]
    [else
     (cons (defS (defS-name (first m))
             (defS-val (first m)))
           (defs-methods (rest m)))])) 

; main desugaring function
(define (desugar [as : ExprS]) : ExprC
  (type-case ExprS as
    [numS (n) (numC n)]
    [plusS (l r) (plusC (desugar l)
                        (desugar r))]
    [multS (l r) (multC (desugar l)
                        (desugar r))]
    [bminusS (l r) (plusC (desugar l)
                          (multC (numC -1) (desugar r)))]
    [idS (i) (idC i)]
    [lamS (args body) (lamC args (desugar body))]
    [appS (f args) (appC (desugar f) (map desugar args))]
    [if0S (c t e) (if0C (desugar c) (desugar t) (desugar e))]
    [withS (bindings body) (appC (lamC (map defS-name bindings) (desugar body)) 
                                 (map desugar (map defS-val bindings)))]
    [seqS (exps) (expand-seqs exps)]
    [setS (var arg) (setC var (desugar arg))]
    [msgidS (sym) (msgidC sym)]
    [varcaseS (var options elsecase) (varcaseC var (map (lambda (opt) 
                                                          (optC (defS-name opt) 
                                                                (desugar (defS-val opt)))) 
                                                        options) 
                                               (desugar elsecase))]
    ; edit sendS
    [sendS (obj msg args) (appC (appC (desugar obj)
                                      (cons (msgidC msg) empty))
                                (map desugar args))] 
    ; edit newS
    [newS (classname args) (appC (classidC classname) (map desugar args))]
    ))

; desugar arbitrary length of seqS into binary seqC
(define (expand-seqs exps)
  (cond [(empty? (rest exps)) (desugar (first exps))]
        [(cons? (rest exps)) (seqC (desugar (first exps))
                                   (expand-seqs (rest exps)))]))

;;;;;;;;;;; DO NOT EDIT BELOW HERE ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;------------------------------------------------------------------------------------

;;;;;;;;;; Surface Syntax ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; type used to capture both with bindings and varcase options
(define-type DefS
  [defS (name : symbol) (val : ExprS)])

(define-type ExprS
  [numS (n : number)]
  [plusS (l : ExprS) (r : ExprS)]
  [bminusS (l : ExprS) (r : ExprS)]
  [multS (l : ExprS) (r : ExprS)]
  [idS (i : symbol)]
  [appS (f : ExprS) (args : (listof ExprS))]
  [if0S (c : ExprS) (t : ExprS) (e : ExprS)]
  [lamS (args : (listof symbol)) (body : ExprS)]
  [withS (bindings : (listof DefS)) (body : ExprS)]
  [seqS (exps : (listof ExprS))]
  [setS (var : symbol) (arg : ExprS)]
  [msgidS (var : symbol)]
  [varcaseS (var : symbol) (options : (listof DefS)) (elsecase : ExprS)]
  [sendS (obj : ExprS) (msg : symbol) (args : (listof ExprS))]
  [newS (classname : symbol) (args : (listof ExprS))]
  )

;;;;;;;;;;; Classes ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; surface syntax for classes
(define-type ClassS
  [classS (name : symbol)
          (parent : symbol) 
          (parent-constr-args : (listof ExprS))
          (constructor-vars : (listof symbol))
          (private-vars : (listof DefS))
          (public-vars : (listof DefS))
          (methods : (listof DefS))])

; interp will take a list of these, mapping class names to classes
(define-type ClassDef
  [classDef (name : symbol)
            (class : Value)])

; extract classdef from list given name of class
(define (lookup-classdef name cdefList) : Value
  (cond [(empty? cdefList) (error 'lookup-classdef (string-append "unbound classname " (to-string name)))]
        [(cons? cdefList) (if (eq? name (classDef-name (first cdefList)))
                              (classDef-class (first cdefList))
                              (lookup-classdef name (rest cdefList)))]))

;;;;;;;;;;;; Parser ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
                [(fun) (lamS (map s-exp->symbol (s-exp->list (second sl))) (parse (third sl)))]
                [(with) (withS (map (lambda (b) 
                                      (let ([bl (s-exp->list b)])
                                        (defS (s-exp->symbol (first bl)) (parse (second bl)))))
                                    (s-exp->list (second sl)))
                               (parse (third sl)))]
                [(seq) (seqS (map parse (rest sl)))]
                [(set) (setS (s-exp->symbol (second sl)) (parse (third sl)))]
                [(new) (newS (s-exp->symbol (second sl)) (map parse (rest (rest sl))))]
                [(send) (sendS (parse (second sl))
                               (s-exp->symbol (third sl))
                               (map parse (rest (rest (rest sl)))))]
                [else ;; must be a fsunction application
                 (appS (idS (s-exp->symbol (first sl)))
                       (map parse (rest sl)))])]
             [else (appS (parse (first sl))
                         (map parse (rest sl)))]))]
    [else (error 'parse "unexpected syntax")]))

; parser for a class sexpression
(define (parse-class [s : s-expression]) : ClassS
  (let ([sl (s-exp->list s)])
    (let ([name (s-exp->symbol (list-ref sl 1))]
          [cvars (s-exp->list (list-ref sl 2))]
          [parentclass (s-exp->symbol (second (s-exp->list (list-ref sl 3))))]
          [parentargs (rest (rest (s-exp->list (list-ref sl 3))))]
          [privvars (s-exp->list (list-ref sl 4))]
          [pubvars (s-exp->list (list-ref sl 5))]
          [methods (cdddddr sl)])
      (case (s-exp->symbol (first sl))
        [(class) (classS name 
                         parentclass
                         (map parse parentargs)
                         (map s-exp->symbol cvars)
                         (parse-members  privvars 'private) 
                         (parse-members pubvars 'public) 
                         (parse-members methods 'methods))]
        [else (error 'parse-class "Presume class definition doesn't start with class")]))))

; parser for public/private/method declarations
; used as a helper in parse-class
(define (parse-members [sexpL : (listof s-expression)] [tag-in-first : symbol]) : (listof DefS)
  (begin
    (unless (or (eq? tag-in-first 'methods) (eq? (s-exp->symbol (first sexpL)) tag-in-first))
      (error 'parse "Malformed class definition"))
    (let ([binds (if (eq? tag-in-first 'methods) 
                     (map s-exp->list sexpL)
                     (map s-exp->list (rest sexpL)))])
      (map (lambda (pr) 
             (defS (s-exp->symbol (first pr))
               (parse (second pr))))
           binds))))

; helper to extract a long tail of a list
(define (cdddddr L) (rest (rest (rest (rest (rest (rest L)))))))


;;;;;;;; abstract syntax ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; abstract syntax
(define-type ExprC
  [numC (n : number)]
  [plusC (l : ExprC) (r : ExprC)]
  [multC (l : ExprC) (r : ExprC)]
  [idC (i : symbol)]
  [appC (f : ExprC) (args : (listof ExprC))]
  [if0C (c : ExprC) (t : ExprC) (e : ExprC)]
  [lamC (args : (listof symbol)) (body : ExprC)]
  [seqC (b1 : ExprC) (b2 : ExprC)]
  [setC (var : symbol) (arg : ExprC)]
  [varcaseC (var : symbol) (options : (listof OptionC)) (elsecase : ExprC)]
  [classidC (i : symbol)]
  [msgidC (msg : symbol)]
  )

;;; Options within a case-like statement ;;;

(define-type OptionC
  [optC (name : symbol) (value : ExprC)])

; a lookup operation on options, used to select which case to evaluate
(define (match-option var options elsecase)
  (cond [(empty? options) elsecase]
        [(cons? options) (if (symbol=? var (optC-name (first options)))
                             (optC-value (first options))
                             (match-option var (rest options) elsecase))]))

;;;;;;;;; output values and interpreter results ;;;;;;;;;;;;;;

(define-type Value
  [numV (n : number)]
  [closV (args : (listof symbol)) (body : ExprC) (env : Env)]
  [msgidV (s : symbol)]
  [dummyV] ; used in settings that require a dummy value that will be ignored or reset before use
  )

(define-type Result
  [v*s (v : Value) (s : Store)])

;;;;;;;;; Environment ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-type Binding
  [bind (name : symbol) (loc : Location)])

(define-type-alias Env (listof Binding))
(define mt-env empty)
(define extend-env cons)

; return first value bound to id in env, or raise error if id is not found
(define (lookup [id : symbol] [env : Env]) : Location
  (cond [(empty? env) (error 'lookup (string-append "unbound identifier " (to-string id)))]
        [(cons? env) (if (symbol=? id (bind-name (first env)))
                         (bind-loc (first env))
                         (lookup id (rest env)))]))

;;;;;;;;; Store ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-type-alias Location number)

(define-type Storage
  [cell (location : Location) (val : Value)])

(define-type-alias Store (listof Storage))
(define mt-store empty)
(define override-store cons)

; generates an unused memory location
; start from 1000 to avoid confusion with small numeric data
(define new-loc
  (let ([n (box 1000)])
    (lambda ()
      (begin
        (set-box! n (add1 (unbox n)))
        (unbox n)))))

; retrieves contents from memory based on location
(define (fetch [loc : Location] [sto : Store]) : Value
  (cond [(empty? sto) (error 'fetch "Memory address out of bounds")]
        [(cons? sto) (if (= loc (cell-location (first sto)))
                         (cell-val (first sto))
                         (fetch loc (rest sto)))]))

;;;;;;;; operators on numVs ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (num+ [l : Value] [r : Value]) : Value
  (cond
    [(and (numV? l) (numV? r))
     (numV (+ (numV-n l) (numV-n r)))]
    [else
     (error 'num+ "type error: one argument was not a number")]))

(define (num* [l : Value] [r : Value]) : Value
  (cond
    [(and (numV? l) (numV? r))
     (numV (* (numV-n l) (numV-n r)))]
    [else
     (error 'num* "type error: one argument was not a number")]))

(define (num0? [v : Value]) : boolean
  (if (numV? v) 
      (zero? (numV-n v))
      (error 'num0? "type error: argument was not a number")))

;;;;;;;; Interpreter ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; the interpreter
(define (interp [a : ExprC] [env : Env] [sto : Store] [classes : (listof ClassDef)]) : Result
  (type-case ExprC a
    [numC (n) (v*s (numV n) sto)]
    [plusC (l r) (type-case Result (interp l env sto classes)
                   [v*s (v-l s-l)
                        (type-case Result (interp r env s-l classes)
                          [v*s (v-r s-r)
                               (v*s (num+ v-l v-r) s-r)])])]
    [multC (l r) (type-case Result (interp l env sto classes)
                   [v*s (v-l s-l)
                        (type-case Result (interp r env s-l classes)
                          [v*s (v-r s-r)
                               (v*s (num* v-l v-r) s-r)])])]
    [idC (i) (v*s (fetch (lookup i env) sto) sto)] 
    [lamC (args body) (v*s (closV args body env) sto)]
    [appC (f args)
          (type-case Result (interp f env sto classes)
            [v*s (v-f s-f)
                 (begin
                   (unless (closV? v-f)
                     (error 'interp "type error: attempt to call something that isn't a function"))
                   (let ([params (closV-args v-f)]
                         [body (closV-body v-f)])
                     (cond [(not (= (length params) (length args)))
                            (begin (display "calling: ") (display v-f) (display #\newline)
                                   (error 'interp "Wrong number of arguments to function"))]
                           [(duplicates? params)
                            (error 'interp (string-append "binding name multiple times: " (to-string params)))]
                           [else (let ([newbinds (map (lambda (p) (bind p (new-loc))) params)])
                                   (type-case Result (interp-args newbinds args env s-f classes)
                                     [v*s (v-arg s-arg)
                                          (interp body
                                                  (append newbinds (closV-env v-f))
                                                  s-arg
                                                  classes)]))])))])]
    [if0C (c t e) (type-case Result (interp c env sto classes)
                    [v*s (v-a s-a)
                         (if (num0? v-a)
                             (interp t env s-a classes)
                             (interp e env s-a classes))])]
    [seqC (b1 b2) (type-case Result (interp b1 env sto classes)
                    [v*s (v-b1 s-b1)
                         (interp b2 env s-b1 classes)])]
    [setC (var val) (type-case Result (interp val env sto classes)
                      [v*s (v-val s-val)
                           (let ([where (lookup var env)])
                             (v*s v-val
                                  (override-store (cell where v-val)
                                                  s-val)))])]
    [classidC (i) (v*s (lookup-classdef i classes) sto)] 
    [msgidC (sym) (v*s (msgidV sym) sto)] 
    [varcaseC (var options elsecase) 
              (let ([msgtext (fetch (lookup var env) sto)])
                (interp (match-option (msgidV-s msgtext) options elsecase) env sto classes))]
    ))

; interpret a list of args left to right, threading the store through each call
; newenv contains the locations in which to store the args (in order)
; returns the store from evaluating the final argument and a dummy value
(define (interp-args newenv args env sto classes) : Result
  (cond [(empty? args) (v*s (dummyV) sto)]
        [(cons? args) (let ([arg-loc (bind-loc (first newenv))])
                        (type-case Result (interp (first args) env sto classes)
                          [v*s (v-arg s-arg)
                               (interp-args (rest newenv) (rest args) env 
                                            (override-store (cell arg-loc v-arg) s-arg)
                                            classes)]))]))

; helper to check for duplicate names
(define (duplicates? lst)
  (cond [(empty? lst) false]
        [(cons? lst) (or (member (first lst) (rest lst))
                         (duplicates? (rest lst)))]))

;;;;;;;;;;;;;;;; API for Running Programs ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-type DefsSto
  [defs*s (defs : (listof ClassDef)) (s : Store)])

; parse-desugar-interp chain for class definitions
(define (process-class-sexps [csexps : (listof s-expression)]) : DefsSto
  (letrec ([loop (lambda (cexps parsedL sto)
                   (if (empty? cexps) (defs*s parsedL sto) 
                       (let* ([s (first cexps)]
                              [parsed (parse-class s)]
                              [classname (classS-name parsed)]
                              [cvalsto (interp (desugar-class parsed) mt-env sto parsedL)])
                         (loop (rest cexps)
                               (cons (classDef classname (v*s-v cvalsto)) parsedL)
                               (v*s-s cvalsto)))))])
    (loop csexps empty mt-store)))


; a run-command that takes program and a list of classes
;   limits time to handle programs with infinite loops
(define (run/classes sexp classes-sexps)
  (call-with-limits 
   10 #f
   (lambda ()
     (type-case DefsSto (process-class-sexps classes-sexps)
       [defs*s (defs sto)
         (v*s-v (interp (desugar (parse sexp)) 
                        mt-env sto 
                        defs))]))))

; preserve the API of our old (pre-classes) test cases
(define (run sexp)
  (run/classes sexp empty))




