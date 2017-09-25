#lang plai-typed

(require (typed-in racket/sandbox [call-with-limits : (number boolean (-> 'a) -> 'a)]))

;; starter file for calc locals assignment, untyped version

;;;;;;;;;; Surface Syntax ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-type ExprS
  [numS (n : number)]
  [boolS (b : boolean)]
  [plusS (l : ExprS) (r : ExprS)]
  [num=S (l : ExprS) (r : ExprS)]
  [idS (i : symbol)]
  [appS (f : ExprS) (arg : ExprS)]
  [bifS (c : ExprS) (t : ExprS) (e : ExprS)]
  [lamS (param : symbol) (body : ExprS)]
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
  [openS (rec : ExprS) (body : ExprS)]
  )

; s type for defining fields in records
(define-type FieldExprS
  [fieldS (name : symbol) (val : ExprS)])

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
                [(fun) (if (= (length sl) 3)                           
                           (let ([pspec (s-exp->list (second sl))])
                             (begin (unless (= (length pspec) 1)
                                      (error 'parse (string-append "parse error: Malformed parameter specification (expected 1 parameter)" (to-string pspec))))
                                    (let ([pname (s-exp->symbol (first pspec))])
                                      (lamS pname (parse (third sl))))))
                           (error 'parse (string-append "parse error: Malformed fun expression" (to-string s))))]
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
                [(open) (openS (parse (second sl)) (parse (third sl)))]
                [else ;; must be a function application
                 (appS (idS (s-exp->symbol (first sl)))
                       (parse (second sl)))])]
             [else (appS (parse (first sl))
                         (parse (second sl)))]))]
    [else (error 'parse (string-append "parse error: unexpected syntax" (to-string s)))]))

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
    [lamS (param body) (calc-locals body (cons param knownNames))]
    [withS (var val body)
           (append (calc-locals val knownNames)
                   (calc-locals body (cons var knownNames)))]
    [holeS () knownNames]
    [recordS (fields) (append (extract-field-val fields knownNames)
                              knownNames)]           
    ; retrieve field value from a record
    [lookupS (rec name) (calc-locals rec knownNames)]                 
    ; add a field to a record
    [extendS (rec name val)             
             (append (calc-locals rec knownNames)
                     (calc-locals val knownNames))]
    ; make record names available in the body (the Javascript with statement)
    [openS (rec body)
           (append (calc-locals rec knownNames) 
                   (calc-locals body                      
                                (append (map (λ (field) (fieldS-name field))
                                             (recordS-fields rec))
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
