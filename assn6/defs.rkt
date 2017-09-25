#lang plai-typed

(define-type Constraints
  [eqCon (lhs : Term) (rhs : Term)])

(define-type Substitution
  [sub [var : Term] [is : Term]])


(define-type Term
  [tExp (e : ExprS)]
  [tVar (s : symbol)]
  [tNum]
  [tBool]
  [tList (elem : Term)]
  [tArrow (dom : Term) (rng : Term)])

(define-type Type
  [numT]
  [boolT]
  [tlistT (elem : Type)]
  [funT (arg : Type) (return : Type)]
  [varT (v : symbol)])

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