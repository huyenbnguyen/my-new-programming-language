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
                         (list (eqCon (tExp f) (tArrow (tExp a) (tExp e)))
                               (eqCon (tExp e) (tVar (gensym 'x)))))]
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
                                      (eqCon (tExp b) (tExp e))))]       
    [recS (var val b) (append3 (generate-constraints val)
                               (generate-constraints b)
                               (list 
                                (eqCon (tExp e) (tExp b))
                                (eqCon (tVar var) (tExp val))))]
    [tconsS (esub l) (let [(x (gensym 'x))]
                       (append3 (generate-constraints esub)
                                (generate-constraints l)
                                (list 
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
                             (list 
                              (eqCon (tExp e) (tList (tVar x)))
                              (eqCon (tExp esub) (tList (tVar x))))))]))



; unify :: <you decide the contract>
(define (unify [cs : (listof Constraints)]) : (listof Constraints)
  (unify/theta cs empty))

(define (lp a)
  (cond ((empty? a) "")
        ((empty? (rest a)) (to-string (first a)))   
        (else (string-append (string-append (to-string (first a)) "\n") (lp (rest a))))))

(define (is-identifier a)
  (or (tVar? a) (tExp? a)))

(define (unify/theta [cs : (listof Constraints)] [theta : (listof Constraints)]) : (listof Constraints)
  (begin ;(display "\n") (display (lp cs)) (display "\n===\n") (display (lp theta)) (display "\n")
    (cond
      [(empty? cs) theta]
      [(cons? cs)
       (let ([l (eqCon-lhs (first cs))]
             [r (eqCon-rhs (first cs))])
         (cond
           [(equal? l r) (unify/theta (rest cs) (cons (first cs) theta))]
           [(is-identifier l) (begin (occurs-check l r)
                                     (unify/theta (replace-in-constraints l r (rest cs))
                                                  (cons (first cs) (replace-in-constraints l r theta))))]
           [(is-identifier r) (begin (occurs-check r l)
                                     (unify/theta (replace-in-constraints r l (rest cs))
                                                  (cons (first cs) (replace-in-constraints r l theta))))]
           [(and (tArrow? l) (tArrow? r))
            (unify/theta (append (list (eqCon (tArrow-dom l) (tArrow-dom r))
                                       (eqCon (tArrow-rng l) (tArrow-dom r)))
                                 (rest cs))
                         theta)]
           [(and (tList? l) (tList? r))
            (unify/theta (cons (eqCon (tList-elem l) (tList-elem r)) (rest cs)) theta)]
           [else (error 'unify/theta "type error")]))])))

; Term Term (listof Constraints) -> (listof Constraints)
(define (replace-in-constraints l r locs)
  (map (λ (e) (eqCon (replace-in-term l r (eqCon-lhs e))
                     (replace-in-term l r (eqCon-rhs e)))) locs))

; Term Term Term -> Term
(define (replace-in-term l r a)
  (cond
    ((equal? l a) r)
    (else
     (type-case Term a
       (tArrow (dom rng) (tArrow (replace-in-term l r dom)
                                 (replace-in-term l r rng)))
       (tList (elem) (tList (replace-in-term l r elem)))
       (else a)))))

(define (occurs-check id term)
  (if (equal? id term)
      (error 'occurs-check "occurs check")
      (type-case Term term
        [tList (elem) (occurs-check id elem)]
        [tArrow (dom rng) (begin (occurs-check id dom)
                                 (occurs-check id rng))]
        [else "literally anything"])))


; type-of :: Expr -> Type
; this will call generate-constraints and unify, in a way that
;  is consistent with your types for these functions
(define (type-of (e : ExprS)) : Type
  (find-type e (unify (generate-constraints e))))

; find-type: ExprS Subst -> Type
; find the type of the given expression in Subst
(define (find-type [e : ExprS] [theta : (listof Constraints)]) : Type
  (begin (display "\n") (display "\n") (display (to-string e)) (display "\n") (display "\n") 
         (cond
           ([empty? theta] (error 'find-type "can't find the expression in Subst"))
           ([cons? theta]
            (if (equal? (tExp e) (eqCon-lhs (first theta)))
                (term-to-type (eqCon-rhs (first theta)) theta)
                (if (equal? (tExp e) (eqCon-rhs (first theta)))
                    (term-to-type (eqCon-lhs (first theta)) theta)             
                    (find-type e (rest theta))))))))

; term-to-type: Term Subst -> Type
; convert Term to Type
(define (term-to-type [term : Term] [theta : (listof Constraints)]) : Type
  (begin ;(display "\n") (display "\n") (display "\n") (display (to-string term)) (display "\n")
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

; (trace unify)