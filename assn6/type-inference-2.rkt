#lang plai-typed

(require (typed-in racket/sandbox [call-with-limits : (number boolean (-> 'a) -> 'a)])
         (typed-in racket/base [gensym : (symbol -> symbol)])
         (typed-in racket/base [ormap : (('a -> boolean) (listof 'a) -> boolean)])
         )

;; starter file for type inference assignment

;; the require above makes the racket ormap function available. It takes a predicate
;;   and a list and returns true iff the predicate holds of some element in the list

;; the require also brings in a function called gensym which produces a fresh
;;   symbol with the same basename as an existing symbol.  It is useful if you
;;   need a fresh variable name during constraint generation

;;;;;;;;;; Surface Syntax ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-type ExprS
  [numS (n : number)]
  [boolS (b : boolean)]
  [temptyS]
  [plusS (l : ExprS) (r : ExprS)]
  [bminusS (l : ExprS) (r : ExprS)]
  [multS (l : ExprS) (r : ExprS)]
  [idS (i : symbol)]
  [appS (f : ExprS) (arg : ExprS)]
  [iszeroS (e : ExprS)]
  [bifS (c : ExprS) (t : ExprS) (e : ExprS)]
  [lamS (param : symbol) (body : ExprS)]
  [withS (var : symbol) (val : ExprS) (body : ExprS)]
  [recS (var : symbol) (val : ExprS) (body : ExprS)]
  [tconsS (e : ExprS) (l : ExprS)]
  [tisEmptyS (e : ExprS)]
  [tfirstS (e : ExprS)]
  [trestS (e : ExprS)]
  )

; the varT type supports type parameters.  For example, the
; identity function has type funT((varT'a) (varT 'a)), where
; 'a is variable over types
(define-type Type
  [numT]
  [boolT]
  [tlistT (elem : Type)]
  [funT (arg : Type) (return : Type)]
  [varT (v : symbol)])

; equality checker for types that supports type variables.
;  two types are equal if there exists a one-to-one mapping between
;  their variable names under which the types become lexically identical
;  (in addition to structurally identical)
(define (type=? (t1 : Type) (t2 : Type)) : boolean
  (local ([define ht1 (make-hash empty)] ; maps vars in t1 to vars in t2
          [define ht2 (make-hash empty)] ; vice versa
          [define (teq? t1 t2)
            (cond
              [(and (numT? t1) (numT? t2)) true]
              [(and (boolT? t1) (boolT? t2)) true]
              [(and (tlistT? t1) (tlistT? t2))
               (teq? (tlistT-elem t1) (tlistT-elem t2))]
              [(and (funT? t1) (funT? t2))
               (and (teq? (funT-arg t1) (funT-arg t2))
                    (teq? (funT-return t1) (funT-return t2)))]
              [(and (varT? t1) (varT? t2))
               ; v1 is the type that ht1 says that t1 maps to, or the var of t2 if no mapping exists
               ; v2 is analogous
               (let ([v1 (let ([r (hash-ref ht1 (varT-v t1))])
                           (if (some? r) 
                               ; var is in the hash, return the mapped value
                               (some-v r)
                               ; else add new mapping to hash and return the newly mapped variable
                               (begin (hash-set! ht1 (varT-v t1) (varT-v t2))
                                      (varT-v t2))))]
                     [v2 (let ([r (hash-ref ht2 (varT-v t2))])
                           (if (some? r)
                               (some-v r)
                               (begin (hash-set! ht2 (varT-v t2) (varT-v t1))
                                      (varT-v t1))))])
                 ; we have to check both mappings, so that distinct variables
                 ; are kept distinct. i.e. a -> b should not be isomorphic to
                 ; c -> c under the one-way mapping a => c, b => c.
                 (and (symbol=? (varT-v t2) v1) (symbol=? (varT-v t1) v2)))]
              [else false])]) ; types aren't equal
    (teq? t1 t2)))

#|
Here are some examples of type=? so you can see what it should do.  Feel free
to delete this from the starter file once you understand type=?

(test (type=? (varT 'a) (varT 'b)) true)

(test (type=? (funT (varT 'a) (varT 'a)) 
              (funT (numT) (numT)))
      false)

(test (type=? (funT (varT 'a) (varT 'a)) 
              (funT (varT 'b) (varT 'b)))
      true)

(test (type=? (funT (varT 'a) (varT 'b)) 
              (funT (varT 'b) (varT 'b)))
      false)

(test (type=? (funT (varT 'a) (varT 'b)) 
              (funT (varT 'b) (varT 'a)))
      true)

(test (type=? (funT (varT 'a) (varT 'a)) 
              (funT (varT 'b) (varT 'a)))
      false)
|#


;;;;;;;;;;;; Parser ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; parses s-expressions into surface syntax
(define (parse [s : s-expression]) : ExprS
  (cond
    [(s-exp-number? s) (numS (s-exp->number s))]
    [(s-exp-symbol? s) 
     (case (s-exp->symbol s)
       [(true) (boolS true)]
       [(false) (boolS false)]
       [(tempty) (temptyS)]
       [else (idS (s-exp->symbol s))])]
    [(s-exp-list? s)
     (let ([sl (s-exp->list s)])
       (cond [(s-exp-symbol? (first sl)) ;; built-in construct or calling function through an identifier
              (case (s-exp->symbol (first sl))
                [(+) (plusS (parse (second sl)) (parse (third sl)))]
                [(*) (multS (parse (second sl)) (parse (third sl)))]
                [(-) (bminusS (parse (second sl)) (parse (third sl)))]
                [(iszero) (iszeroS (parse (second sl)))]
                [(bif) (bifS (parse (second sl)) (parse (third sl)) (parse (fourth sl)))]
                [(fun) (lamS (s-exp->symbol (first (s-exp->list (second sl)))) (parse (third sl)))]
                [(with) (let ([bindings (s-exp->list (second sl))]
                              [body (third sl)])
                          (begin (unless (= 1 (length bindings))
                                   (error 'parse (string-append "parse error: with expects list containing one binding but got " (to-string bindings))))
                                 (let ([binding (s-exp->list (first bindings))])
                                   (withS (s-exp->symbol (first binding))
                                          (parse (second binding))
                                          (parse body)))))]
                [(rec) (let ([bindings (s-exp->list (second sl))]
                             [body (third sl)])
                         (recS (s-exp->symbol (first bindings))
                               (parse (second bindings))
                               (parse body)))]
                [(tcons) (tconsS (parse (second sl)) (parse (third sl)))]
                [(tempty?) (tisEmptyS (parse (second sl)))]
                [(tfirst) (tfirstS (parse (second sl)))]
                [(trest) (trestS (parse (second sl)))]
                [else ;; must be a function application
                 (appS (idS (s-exp->symbol (first sl)))
                       (parse (second sl)))])]
             [else (appS (parse (first sl))
                         (parse (second sl)))]))]
    [else (error 'parse "parse error: unexpected syntax")]))

;;;;;;;;;;;; Rename Vars ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; You may find this useful if you want to write programs that use the same var
;; name in multiple scopes -- it will automatically make the names unique

(define-type Rename
  [renamebind (old : symbol) (new : symbol)])

(define (lookup-alpha oldname renameList)
  (let ([res (filter (lambda (rn) (eq? oldname (renamebind-old rn))) renameList)])
    (if (cons? res) 
        (renamebind-new (first res))
        oldname)))

; alpha-renaming so that all var names are unique 
(define (alpha-rename [renamings : (listof Rename)] [expr : ExprS]) : ExprS
  (type-case ExprS expr
    [numS (n) expr]
    [boolS (v) expr]
    [plusS (l r) (plusS (alpha-rename renamings l) (alpha-rename renamings r))]
    [multS (l r) (multS (alpha-rename renamings l) (alpha-rename renamings r))]
    [bminusS (l r) (bminusS (alpha-rename renamings l) (alpha-rename renamings r))]
    [iszeroS (t) (iszeroS (alpha-rename renamings t))]
    [bifS (t ifdo elsedo) (bifS (alpha-rename renamings t) (alpha-rename renamings ifdo) (alpha-rename renamings elsedo))]
    [idS (name) (idS (lookup-alpha name renamings))]
    [withS (param val body)
           (let ([newparam (gensym param)])
             (withS newparam (alpha-rename renamings val) 
                    (alpha-rename (cons (renamebind param newparam) renamings) body)))]
    [recS (param val body)
          (let* ([newparam (gensym param)]
                 [newL (cons (renamebind param newparam) renamings)])
            (recS newparam (alpha-rename newL val) 
                  (alpha-rename newL body)))]
    [lamS (param body)
          (let ([newparam (gensym param)])
            (lamS newparam (alpha-rename (cons (renamebind param newparam) renamings) body)))]
    [appS (f val) (appS (alpha-rename renamings f) (alpha-rename renamings val))]
    [temptyS () expr]
    [tconsS (f r) (tconsS (alpha-rename renamings f) (alpha-rename renamings r))]
    [tisEmptyS (l) (tisEmptyS (alpha-rename renamings l))]
    [tfirstS (l) (tfirstS (alpha-rename renamings l))]
    [trestS (l) (trestS (alpha-rename renamings l))]))

;;;;;;;;;;;;;; type inferrer ;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; feel free to add any other helpers that you deem necessary

;
#|
(define-type ExprS
  [numS (n : number)]
  [boolS (b : boolean)]
  [temptyS]
  [plusS (l : ExprS) (r : ExprS)]
  [bminusS (l : ExprS) (r : ExprS)]
  [multS (l : ExprS) (r : ExprS)]
  [idS (i : symbol)]
  [appS (f : ExprS) (arg : ExprS)]
  [iszeroS (e : ExprS)]
  [bifS (c : ExprS) (t : ExprS) (e : ExprS)]
  [lamS (param : symbol) (body : ExprS)]
  [withS (var : symbol) (val : ExprS) (body : ExprS)]
  [recS (var : symbol) (val : ExprS) (body : ExprS)]
  [tconsS (e : ExprS) (l : ExprS)]
  [tisEmptyS (e : ExprS)]
  [tfirstS (e : ExprS)]
  [trestS (e : ExprS)]
  )

; the varT type supports type parameters.  For example, the
; identity function has type funT((varT'a) (varT 'a)), where
; 'a is variable over types
(define-type Type
  [numT]
  [boolT]
  [tlistT (elem : Type)]
  [funT (arg : Type) (return : Type)]
  [varT (v : symbol)])
|#

(define-type Constraints
  [eqCon (lhs : Term) (rhs : Term)])


(define-type Term
  [tExp (e : ExprS)]
  [tVar (s : symbol)]
  [tNum]
  [tBool]
  [tList (elem : Term)]
  [tArrow (dom : Term) (rng : Term)])


; generate-constraints :: <you decide the contract>
(define (generate-constraints [e : ExprS]) : (listof Constraints)
  (type-case ExprS e
    [numS (n) (list (eqCon (tExp e) (tNum)))]
    [boolS (b) (list (eqCon (tExp e) (tBool)))]
    [temptyS () (list (eqCon (tExp e) (tList (tVar (gensym 'x)))))]
    [plusS (l r) (append3 (generate-constraints l)
                          (generate-constraints r)
                          (list (eqCon (tExp l) (tNum))
                                (eqCon (tExp r) (tNum))
                                (eqCon (tExp e) (tNum))))]
    [bminusS (l r) (append3 (generate-constraints l)
                            (generate-constraints r)
                            (list (eqCon (tExp l) (tNum))
                                  (eqCon (tExp r) (tNum))
                                  (eqCon (tExp e) (tNum))))]
    [multS (l r) (append3 (generate-constraints l)
                          (generate-constraints r)
                          (list (eqCon (tExp l) (tNum))
                                (eqCon (tExp r) (tNum))
                                (eqCon (tExp e) (tNum))))]
    [idS (i) (list (eqCon (tExp e) (tVar i)))]
    [appS (f a) (append3 (generate-constraints f)
                         (generate-constraints a)
                         (list (eqCon (tExp f) (tArrow (tExp a) (tExp e)))))]
    [iszeroS (esub) (append (generate-constraints esub)
                            (list (eqCon (tExp e) (tBool))
                                  (eqCon (tExp esub) (tNum))))]
    [bifS (c t esub) (append4 (generate-constraints c) 
                              (generate-constraints t)
                              (generate-constraints esub)
                              (list (eqCon (tExp c) (tBool))
                                    (eqCon (tExp t) (tExp esub))
                                    (eqCon (tExp e) (tExp t))
                                    (eqCon (tExp e) (tExp esub))))]  
    [lamS (p b) (append (generate-constraints b)
                        (list (eqCon (tExp e)
                                     (tArrow (tVar p) (tExp b)))))]
    [withS (var val b) (append3 (generate-constraints val)
                                (generate-constraints b)
                                (list (eqCon (tVar var) (tExp val))
                                      (eqCon (tExp b) (tArrow (tExp val) (tExp e)))))]       
    [recS (var val b) (append3 (generate-constraints val)
                               (generate-constraints b)
                               (list (eqCon (tExp b) (tArrow (tExp val) (tExp e)))
                                     (eqCon (tExp val) (tVar var))))]
    [tconsS (esub l) (let [(x (gensym 'x))]
                       (append3 (generate-constraints esub)
                                (generate-constraints l)
                                (list ;(eqCon (tExp esub) (tList (tExp l)))
                                 ;(eqCon (tExp l) (tList (tExp e)))
                                 (eqCon (tExp e) (tList (tVar x)))
                                 (eqCon (tExp esub) (tVar x))
                                 (eqCon (tExp l) (tList (tVar x))))))]
    [tisEmptyS (esub) (append (list (eqCon (tExp e) (tBool)))
                              (list (eqCon (tExp esub) (tList (tVar (gensym 'x))))))]
    [tfirstS (esub) (let ((x (gensym 'x)))
                      (append3
                       (generate-constraints esub)
                       (list (eqCon (tExp e) (tVar x)))
                       (list (eqCon (tExp esub) (tList (tVar x))))))]
    [trestS (esub) (let [(x (gensym 'x))]
                     (append (generate-constraints esub)
                             (list ; (eqCon (tExp e) (tList (tExp esub)))
                              (eqCon (tExp e) (tList (tVar x)))
                              (eqCon (tExp esub) (tList (tVar x))))))]))



; unify :: <you decide the contract>
(define (unify [cs : (listof Constraints)]) : Subst
  (unify/theta cs empty))


(define-type-alias Subst (listof Substitution))

(define-type Substitution
  [sub [var : Term] [is : Term]])

(define (lp a)
  (cond ((empty? a) "")
        ((empty? (rest a)) (to-string (first a)))
        (else (string-append (string-append (to-string (first a)) "\n") (lp (rest a))))))

(define (unify/theta [cs : (listof Constraints)] [theta : Subst]) : Subst
  (begin (display "\n") (display (lp cs)) (display "\n===\n") (display (lp theta)) (display "\n")
         (cond
           [(empty? cs) theta]
           [(cons? cs)
            (let ([l (eqCon-lhs (first cs))]
                  [r (eqCon-rhs (first cs))])
              (cond
                [(equal? l r) (unify/theta (rest cs) theta)]
                [else 
                 (type-case Term l
                   [tVar (s) (type-case (optionof Term) (lookup l theta)
                               [some (bound)
                                     (unify/theta (cons (eqCon bound r)
                                                        (replace-in-constraints l r (rest cs)))
                                                  theta)]
                               [none () 
                                     (unify/theta (replace-in-constraints l r (rest cs))
                                                  (extend+replace l r theta))])]
                   [tExp (s) (begin
                               (type-case (optionof Term) (lookup l theta)
                                 [some (bound)
                                       (unify/theta (cons (eqCon bound r)
                                                          (replace-in-constraints l r (rest cs)))
                                                    theta)]
                                 [none ()
                                       (begin ;(display l) (display "\n")
                                         (unify/theta  (replace-in-constraints l r (rest cs))
                                                       (extend+replace l r theta)))]))]
                   [tNum () (type-case Term r
                              [tNum () (unify/theta (replace-in-constraints l r (rest cs)) theta)]
                              ;[else (error 'unify/theta "type error: tNum and something else")])]
                              [else (unify/theta (replace-in-constraints l r (rest cs)) (extend-theta (first cs) theta))])]
                   [tBool () (type-case Term r
                               [tBool () (unify/theta (replace-in-constraints l r (rest cs)) theta)]
                               [else (error 'unify/theta "type error: tBool and something else")])]
                   [tArrow (dom rng) (type-case Term r
                                       [tArrow (d2 r2)
                                               (unify/theta (cons (eqCon dom d2)
                                                                  (cons (eqCon rng r2)
                                                                        (rest cs)))
                                                            theta)]
                                       [else (error 'unify/theta "type error: tArrow and something else")])]
                   [tList (e1) (type-case Term r
                                 [tList (e2) (unify/theta (cons (eqCon e1 e2) (rest cs)) theta)]
                                 [else (error 'unify/theta "type error: tList and something else")])])]))])))
;[else (error 'unify/theta "tList / haven't handled this case yet")])]))])))

(define (extend-theta [cs : Constraints] [theta : Subst]) : Subst
  (cons (sub (eqCon-lhs cs) (eqCon-rhs cs))
        theta))


; extend+replace: Term Term Subst -> Subst
; perform the occurs test and, if it fails (i.e., there is no circularity),
; extends the substitution and replaces all existing instances of the
; first term with the second in the substitution.
(define (extend+replace [l : Term] [r : Term] [theta : Subst]) : Subst
  (if (occurs-check l r)
      (error 'extend+replace "type error: circularity detected")
      (begin (replace-in-subst l r theta)
             (cons (sub l r) theta))))

(define (replace-in-constraints [l : Term] [r : Term] [cs : (listof Constraints)]) : (listof Constraints)
  (cond
    ([empty? cs] empty)
    ([cons? cs]
     (cond
       [(equal? l (eqCon-lhs (first cs)))             
        (cons (eqCon r (eqCon-rhs (first cs)))
              (replace-in-constraints l r (rest cs)))]
       [(equal? l (eqCon-rhs (first cs)))
        (cons (eqCon (eqCon-lhs (first cs)) r)
              (replace-in-constraints l r (rest cs)))]  
       [(tArrow? (eqCon-lhs (first cs))) (cons (eqCon (replace-in-tArrow l r (eqCon-lhs (first cs)))
                                                      (eqCon-rhs (first cs)))
                                               (replace-in-constraints l r (rest cs)))]
       [(tArrow? (eqCon-rhs (first cs))) (begin ;(display (first cs)) (display "\n")
                                           (cons (eqCon (eqCon-lhs (first cs))
                                                        (replace-in-tArrow l r (eqCon-rhs (first cs))))
                                                 (replace-in-constraints l r (rest cs))))]
       [else  (cons (first cs)
                    (replace-in-constraints l r (rest cs)))]
       ))))

;
(define (replace-in-tArrow [l : Term] [r : Term] [term : Term]) : Term
  (begin ;(display "\n") (display "\n") (display (to-string l)) (display "\n") (display (to-string r)) (display "\n") (display (to-string term)) (display "\n") (display "\n")
    (cond
      ([equal? l (tArrow-dom term)] (tArrow r (tArrow-rng term)))
      ([equal? l (tArrow-rng term)] (tArrow (tArrow-dom term) r)) 
      ;([equal? r (tArrow-dom term)] (tArrow l (tArrow-rng term)))
      ;([equal? r (tArrow-rng term)] (tArrow (tArrow-dom term) l))
      (else term))))

;
(define (replace-in-subst [l : Term] [r : Term] [theta : Subst]) : Subst
  (cond
    [(empty? theta) empty]
    [(cons? theta)
     (begin ;(display "\n") (display "\n") (display (to-string l)) (display "\n") (display (to-string r)) (display "\n") (display (to-string theta)) (display "\n") (display "\n")    
       (if (equal? l (sub-var (first theta)))
           (cons (sub r (sub-is (first theta)))
                 (replace-in-subst l r (rest theta)))
           (if (equal? l (sub-is (first theta)))
               (cons (sub (sub-var (first theta)) r)
                     (replace-in-subst l r (rest theta)))
               (replace-in-subst l r (rest theta)))))]))

; occurs-check: Term Term -> Boolean
; performs the occurs test
; return true if there is no circularity
; consider 4 cases
; 1. l: tArrow, r: tArrow
; 2. l: tArrow, r: tList
; 3. l: tList, r: tArrow
; 4. l: tList, r: tList
(define (occurs-check [l : Term] [r : Term]) : boolean
  (cond
    ((and (tArrow? l) (tArrow? r)) (if (or (equal? (tArrow-dom l) (tArrow-dom r))
                                           (equal? (tArrow-dom l) (tArrow-rng r))
                                           (equal? (tArrow-rng l) (tArrow-dom r))
                                           (equal? (tArrow-rng l) (tArrow-rng r)))
                                       true
                                       false))
    ((and (tArrow? l) (tList? r)) (if (or (equal? (tArrow-dom l) (tList-elem r))
                                          (equal? (tArrow-rng l) (tList-elem r)))
                                      true
                                      false))
    ((and (tList? l) (tArrow? r)) (if (or (equal? (tArrow-dom r) (tList-elem l))
                                          (equal? (tArrow-rng r) (tList-elem l)))
                                      true
                                      false))
    ((and (tList? l) (tList? l)) (if (equal? (tList-elem l) (tList-elem r))
                                     true
                                     false))
    (else false)))
                                                    
;(define (occurs-check [l : Term] [r : Term]) : boolean
;  false)

; lookup: Term Subst -> optionof Term
; look up the term in Subst
(define (lookup [l : Term] [theta : Subst]) : (optionof Term)
  (cond
    [(empty? theta) (none)]
    [(cons? theta)
     (if (equal? l (sub-var (first theta)))
         (some (sub-is (first theta)))
         (lookup l (rest theta)))]))

; type-of :: Expr -> Type
; this will call generate-constraints and unify, in a way that
;  is consistent with your types for these functions
(define (type-of (e : ExprS)) : Type
  (find-type e (unify (generate-constraints e))))

; find-type: ExprS Subst -> Type
; find the type of the given expression in Subst
(define (find-type [e : ExprS] [theta : Subst]) : Type
  (begin (display "\n") (display "\n") (display (to-string e)) (display "\n") (display "\n") 
         (cond
           ([empty? theta] (error 'find-type "can't find the expression in Subst"))
           ([withS? e] (find-withS-type e theta))
           ([cons? theta]
            (if (equal? (tExp e) (sub-var (first theta)))
                (term-to-type (sub-is (first theta)) theta)
                (if (equal? (tExp e) (sub-is (first theta)))
                    (term-to-type (sub-var (first theta)) theta)             
                    (find-type e (rest theta))))))))

; find-withS-type: ExprS Subst -> Type
; special function to find the type of withS 
(define (find-withS-type [e : ExprS] [theta : Subst]) : Type
  (cond
    ([empty? theta] (error 'find-withS-type "can't find withS in Subst"))
    ([cons? theta]
     (if (tArrow? (sub-var (first theta)))
         (find-withS-in-tArrow e (sub-var (first theta)) theta)
         (if (tArrow? (sub-is (first theta)))
             (find-withS-in-tArrow e (sub-is (first theta)) theta)
             (find-withS-type e (rest theta)))))))

; find-withS-in-tArrow: ExprS Term Subst -> Type
; find e in given term, which is of type tArrow
; if found, return its type
(define (find-withS-in-tArrow [e : ExprS] [term : Term] [theta : Subst]) : Type
  (cond
    [(equal? (tExp e) (tArrow-dom term)) (term-to-type (tArrow-rng term) theta)]
    [(equal? (tExp e) (tArrow-rng term)) (term-to-type (tArrow-dom term) theta)]))



; term-to-type: Term Subst -> Type
; convert Term to Type
(define (term-to-type [term : Term] [theta : Subst]) : Type
  (begin (display "\n") (display "\n") (display "\n") (display (to-string term)) (display "\n")
         (type-case Term term
           [tExp (e) (error 'term-to-type "can't convert tExp to a Type")]
           [tVar (s) (varT s)]
           [tNum () (numT)]
           [tBool () (boolT)]
           [tList (e) (tlistT (term-to-type e theta))]
           [tArrow (d r) (funT (term-to-type d theta) (term-to-type r theta))])))

; append3 : (listof Constraints) * (listof Constraints) * (listof Constraints) -> (listof Constraints)
; three-argument version of append
(define (append3 [l1 : (listof Constraints)] [l2 : (listof Constraints)] [l3 : (listof Constraints)]) : (listof Constraints)
  (append l1 (append l2 l3)))

; append4 : (listof Constraints) * (listof Constraints) * (listof Constraints) * (listof Constraints) -> (listof Constraints)
; four-argument version of append
(define (append4 [l1 : (listof Constraints)] [l2 : (listof Constraints)] [l3 : (listof Constraints)] [l4 : (listof Constraints)]) : (listof Constraints)
  (append l1 (append l2 (append l3 l4))))

;;;;;;;;;;;;; API for type checking programs ;;;;;;;;;;;

(define (infer-type sexp)
  (call-with-limits 
   10 #f
   ; if you prefer to work with original names, uncomment next line and comment
   ;    out the one after that
   ;(lambda () (type-of (parse sexp)))
   (lambda () (type-of (alpha-rename empty (parse sexp))))
   ))