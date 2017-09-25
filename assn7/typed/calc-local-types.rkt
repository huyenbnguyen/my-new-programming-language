#lang plai-typed

(require (typed-in racket/sandbox [call-with-limits : (number boolean (-> 'a) -> 'a)]))

;; starter file for calc locals assignment, typed version

;;;;;;;;;; Surface Syntax ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-type ExprS
  [numS (n : number)]
  [boolS (b : boolean)]
  [plusS (l : ExprS) (r : ExprS)]
  [num=S (l : ExprS) (r : ExprS)]
  [idS (i : symbol)]
  [appS (f : ExprS) (arg : ExprS)]
  [bifS (c : ExprS) (t : ExprS) (e : ExprS)]
  [lamS (param : symbol) (paramtype : Type) (rettype : Type) (body : ExprS)]
  [withS (var : symbol) (val : ExprS) (body : ExprS)]
  ;; "holes" in expressions
  [holeS]
  ;;; record operators
  ; create a record
  [recordS (fields : (listof FieldExprS))]
  ; retrieve field value from a record
  [lookupS (rec : ExprS) (name : symbol)]
  ; add a field to a record
  [extendS (rec : ExprS) (name : symbol) (val : ExprS)]
  ; make record names available in the body (the Javascript with statement)
  [openS (rec : ExprS) (recType : Type) (body : ExprS)]
  )

; s type for defining fields in records
(define-type FieldExprS
  [fieldS (name : symbol) (val : ExprS)])

(define-type Type
  [numT]
  [boolT]
  [funT (input : Type) (return : Type)]
  [recT (fieldTypes : (listof FieldType))]
  )

(define-type FieldType
  [fieldT (name : symbol) (type : Type)])

;;;;;;;;;;;; Parser ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; parses s-expressions into surface syntax
(define (parse [s : s-expression]) : ExprS
  (cond
    [(s-exp-number? s) (numS (s-exp->number s))]
    [(s-exp-symbol? s) 
     (case (s-exp->symbol s)
       [(true) (boolS true)]
       [(false) (boolS false)]
       [(@) (holeS)]
       [else (idS (s-exp->symbol s))])]
    [(s-exp-list? s)
     (let ([sl (s-exp->list s)])
       (cond [(s-exp-symbol? (first sl)) ;; built-in construct or calling function through an identifier
              (case (s-exp->symbol (first sl))
                [(+) (plusS (parse (second sl)) (parse (third sl)))]
                [(num=) (num=S (parse (second sl)) (parse (third sl)))]
                [(bif) (bifS (parse (second sl)) (parse (third sl)) (parse (fourth sl)))]
                [(fun) (cond [(= (length sl) 5) ;; this case not in official grammar but was left in original file
                              (let ([pspec (s-exp->list (second sl))])
                                (begin (unless (= (length pspec) 3)
                                         (error 'parse (string-append "parse error: Malformed parameter specification (expected 3 parts)" (to-string pspec))))
                                       (unless (eq? ': (s-exp->symbol (third sl)))
                                         (error 'parse (string-append "parse error: Missing : for output type " (to-string sl))))
                                       (unless (eq? ': (s-exp->symbol (second pspec)))
                                         (error 'parse (string-append "parse error: Expected : as second element of parameter syntax in " (to-string pspec))))
                                       (let ([pname (s-exp->symbol (first pspec))]
                                             [ptype (parse-type (third pspec))])
                                         (lamS pname ptype
                                               (parse-type (fourth sl))
                                               (parse (fourth (rest sl)))))))]
                             [(= (length sl) 3) ;; this is the case in the official grammar
                              (let ([pspec (s-exp->list (second sl))])
                                (begin (unless (= (length pspec) 3)
                                         (error 'parse (string-append "parse error: Malformed parameter specification (expected 3 parts)" (to-string pspec))))
                                       (unless (eq? ': (s-exp->symbol (second pspec)))
                                         (error 'parse (string-append "parse error: Expected : as second element of parameter syntax in " (to-string pspec))))
                                       (let ([pname (s-exp->symbol (first pspec))]
                                             [ptype (parse-type (third pspec))])
                                         (lamS pname ptype
                                               (numT) ;; dummy value to fit exprS grammar as defined
                                               (parse (third sl))))))]                              
                             [else
                              (error 'parse (string-append "parse error: Malformed fun expression" (to-string s)))])]
                [(with) (let ([bindings (s-exp->list (second sl))]
                              [body (third sl)])
                          (begin (unless (= 1 (length bindings))
                                   (error 'parse (string-append "parse error: with expects list containing one binding but got " (to-string bindings))))
                                 (let ([binding (s-exp->list (first bindings))])
                                   (withS (s-exp->symbol (first binding))
                                          (parse (second binding))
                                          (parse body)))))]
                [(record) (let ([fields (rest sl)])
                            (recordS (map (lambda (f)
                                            (fieldS (s-exp->symbol (first f))
                                                    (parse (second f))))
                                          (map s-exp->list fields))))]
                [(lookup) (lookupS (parse (second sl)) (s-exp->symbol (third sl)))]
                [(extend) (extendS (parse (second sl)) (s-exp->symbol (third sl)) (parse (fourth sl)))]
                [(open)
                 (if (eq? ': (s-exp->symbol (third sl)))
                     (openS (parse (second sl)) (parse-type (fourth sl)) (parse (fourth (rest sl))))
                     (error 'parse (string-append "parse error: Expected : as third element of open statement in " (to-string sl))))]
                [else ;; must be a function application
                 (appS (idS (s-exp->symbol (first sl)))
                       (parse (second sl)))])]
             [else (appS (parse (first sl))
                         (parse (second sl)))]))]
    [else (error 'parse (string-append "parse error: unexpected syntax" (to-string s)))]))

; parses type annotations in the surface syntax grammar
;     type ::= number
;            | boolean
;            | (type -> type)
(define (parse-type [t : s-expression]) : Type
  (cond [(s-exp-symbol? t)
         (case (s-exp->symbol t)
           [(number) (numT)]
           [(boolean) (boolT)]
           [else (error 'parse-type (string-append "parse error: unrecognized type name " (to-string t)))])]
        [(s-exp-list? t)
         (let ([tl (s-exp->list t)])
           (case (s-exp->symbol (first tl))
             [(rectype) (recT (map (lambda (ft)
                                     (if (eq? ': (s-exp->symbol (second ft)))
                                         (fieldT (s-exp->symbol (first ft)) (parse-type (third ft)))
                                         (error 'parse (string-append
                                                        "parse error: Expected : as second element of field type in "
                                                        (to-string ft)))))                         
                                   (map s-exp->list (rest tl))))]
             [else ; must be a function type
              (if (= (length tl) 3)
                  (let ([tin (parse-type (first tl))]
                        [tarrow (s-exp->symbol (second tl))]
                        [tout (parse-type (third tl))])
                    (if (eq? tarrow '->)
                        (funT tin tout)
                        (error 'parse-type (string-append "parse error: Malformed type syntax " (to-string tl)))))
                  (error 'parse-type  (string-append "parse error: Malformed type syntax " (to-string tl))))]))]
        [else (error 'parse-type  (string-append "parse error: Malformed type syntax " (to-string t)))]))

;;;;;;;;;;;;; Calc Locals ;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (calc-locals (e : ExprS) (knownNames : (listof symbol))) : (listof symbol)
  
  (type-case ExprS e
    [numS (n) empty]
    [boolS (b) empty]
    [plusS (l r) (append (calc-locals l knownNames)
                         (calc-locals r knownNames))]
    [num=S (l r) (append (calc-locals l knownNames)
                         (calc-locals r knownNames))]
    [idS (i) empty]
    [appS (f arg) (calc-locals f (calc-locals arg knownNames))]
    [bifS (c t e) (append (calc-locals t (calc-locals c knownNames))
                          (calc-locals e (calc-locals c knownNames)))]
    [lamS (param paramtype rettype body) (calc-locals body (cons param knownNames))]
    [withS (var val body) (append (calc-locals body (cons var knownNames))
                                  (calc-locals val knownNames))]
    ;; "holes" in expressions
    [holeS () knownNames]
    ;;; record operators
    ; create a record
    [recordS (fields) (append (extract-field-val fields knownNames)
                              knownNames)]
    ; retrieve field value from a record
    [lookupS (rec name)  (if (symbol=? '@ name)
                             (calc-locals (holeS) knownNames)
                             (calc-locals rec knownNames))] 
    ; add a field to a record
    [extendS (rec name val) (append (calc-locals rec knownNames)
                                    (calc-locals val knownNames))]
    ; make record names available in the body (the Javascript with statement)
    [openS (rec recType body)
           (append (calc-locals rec knownNames)
                   (calc-locals body (append (map (λ (fieldType) (fieldT-name fieldType))
                                                  (recT-fieldTypes recType))
                                             knownNames)))]
    ))

; extract-field-val : (listof FieldExprS) (listof symbol) -> (listof symbol)
; extract the list of bounded symbols for the values inside record
(define (extract-field-val fields knownNames)
  (cond
    ((empty? fields) empty)
    (else
     (append
      (calc-locals (fieldS-val (first fields)) knownNames)
      (extract-field-val (rest fields) knownNames)))))

;;;;;;;;;;;;; API for calculating locals  ;;;;;;;;;;;

(define (parse-and-calc sexp) : (listof symbol)
  (call-with-limits 
   10 #f
   (lambda () (calc-locals (parse sexp) empty))))

;(trace calc-locals)