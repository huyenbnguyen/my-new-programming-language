#lang plai-typed

(require "objects.rkt")

;---------------------------------------------
; class definitions
;---------------------------------------------
(define AddClass '(class Adder (first)
                    (parent Object)
                    (private)
                    (public (v 6) (q 5))
                    (add (fun (param) (+ param first)))
                    (subpub (fun () (- first v)))))

(define SubClass '(class Subber (first)
                    (parent Adder (+ first 1))
                    (private (h 2))
                    (public (t 7))
                    (sub (fun (x) (- x h)))))

(define MultClass '(class Multiplier (first)
                     (parent Subber (+ first 1))
                     (private)
                     (public (public1 2))
                     (doublek (fun () (* 2 first)))
                     (mult (fun (param) (* first param)))
                     (sub (fun (param) 3))
                     (inhe (fun () (send parent set-t 8)))))

(define MultiCons1 '(class MultiConstructor1 (first second)
                      (parent Object)
                      (private)
                      (public)
                      (total (fun () (+ first (+ second 3))))))


(define MultiCons2 '(class MultiConstructor2 (first second)
                      (parent MultiConstructor1 (+ 1 first) (+ 1 second))
                      (private)
                      (public)
                      (total (fun () (+ first second)))))

;---------------------------------------------
; basic test cases
;---------------------------------------------

; a subclass can use a getter to get
; the value of a public variable of the parent class
(test (run/classes '(with ((myobj (new Subber 200)))                           
                          (send myobj get-q))                               
                   (list AddClass SubClass))
      (numV 5))

; the object can use getters to get the value of
; the public variables of parent classes
(test (run/classes '(with ([multobj (new Multiplier 2)])
                          (send multobj inhe))
                   (list AddClass SubClass MultClass))
      (numV 8))


;---------------------------------------------
; test cases focus on scoping and mutation
;---------------------------------------------

; test inheritance
; the object can get a parent's method if
; that method hasn't been defined in itself
; even if the method is in the parent's parent class

(test (run/classes '(with ([multobj (new Multiplier 2)])
                          (send multobj add 3))
                   (list AddClass SubClass MultClass))
      (numV 7))

; static binding test
; unbound error due to scoping
; cause: try to reference an identifier
; that hasn't been bounded yet
(test/exn (run/classes '(with ([x (new Multiplier 10)] [y x])
                              (+ (send x mult 2)
                                 (send y mult 2)))
                       (list AddClass SubClass MultClass))
          "unbound")

; static binding test
; choosing between different definitions of an identifier
(test (run/classes '(with ([x (new Multiplier 10)]
                           [g (fun (x) (+ (send x add 2) 5))])
                          (with ([f (fun (y) (+ y (g (new Multiplier 5))))])
                                (with ([g (fun (x) (+ (send x add 100) 100000))]
                                       [x (new Multiplier 5)])
                                      (f 5))))
                   (list AddClass SubClass MultClass))
      (numV 19))

; static binding test
; a identifier can be defined through
; static scoping, or by parameter passing
(test (run/classes '(with ([x (new Multiplier 10)])
                          (with 
                           ([g (fun (x) (+ (send x add 2) 5))])
                           (with ([f (fun (y) (+ y (g (new Multiplier 5))))])
                                 (with ([g (fun (x) (+ (send x add 100) 100000))]
                                        [x (new Multiplier 5)])
                                       (f 5)))))
                   (list AddClass SubClass MultClass))
      (numV 19))

; test shadowing
(test (run/classes '(with ([x (new Multiplier 10)])
                          (with ([x (new Multiplier 20)])
                                (send x mult 2)))
                   (list AddClass SubClass MultClass))
      (numV 40))

; test mutation - basic
(test (run/classes '(with ([x (new Multiplier 10)])
                          (+ (seq (send x inhe)
                                  3)
                             (send x get-t)))
                   (list AddClass SubClass MultClass))
      (numV 11))

; lazy evaluation
(test (run/classes '(with ([x (new Multiplier 10)])
                          (with ([f (fun (a) (send x mult 3))])
                                (f (send x doublek))))
                   (list AddClass SubClass MultClass))
      (numV 30))

; test mutation
; creating 2 objects means each object is getting
; a separate copy of the public variables
; so modifying the public variables of 1 object
; doesn't affect the public variables of the other
(test (run/classes '(with ([x (new Multiplier 10)])
                          (with ([x (new Multiplier 10)]
                                 [y (new Subber 20)])
                                (seq (send x inhe)
                                     (+ (send x get-t)
                                        (send y get-t)))))
                   (list AddClass SubClass MultClass))
      (numV 15))

; test mutation
; when 2 identifiers represent same object
; and we try to modify the public variable using 1 identifier,
; the change will also be reflected in the other identifier
; because they actually point to the same object
(test (run/classes '(with ([x (new Multiplier 10)])
                          (with ([y x])
                                (seq (send x inhe)
                                     (send y get-t))))
                   (list AddClass SubClass MultClass))
      (numV 8))

; test mutation
; when an object is passed to a function,
; it is passed by reference
; so changes made in the function body to the object
; persists outside the function
(test (run/classes '(with ([f (fun (x) (send x inhe))])
                          (with ([x (new Multiplier 5)])
                                (seq
                                 (f x)
                                 (send x get-t))))
                   (list AddClass SubClass MultClass)) 
      (numV 8))

; test scoping
; test if the language is capable of grabbing
; identifiers at different levels of nested withs
(test (run/classes '(with ([a (new Subber 10)] [b (new Adder 20)])
                          (with ([c (new Multiplier 30)])
                                (with ([a (new Adder 5)] [b (new Subber 3)])
                                      (+
                                       (+ (send a get-v)
                                          (send c get-v))
                                       (send b get-t)))))
                   (list AddClass SubClass MultClass)) 
      (numV 19))

; test inheritance
; if the sub-class has the same method
; as the parent class
; the sub-class should use its own method
(test (run/classes '(with ([x (new MultiConstructor2 10 10)])
                          (send x total))
                   (list MultiCons1 MultiCons2))
      (numV 20))

; test inheritance
; if the sub-class has the same method
; as the parent class
; the parent class should use its own method
(test (run/classes '(with ([x (new MultiConstructor1 10 10)])
                          (send x total))
                   (list MultiCons1 MultiCons2))
      (numV 23))

























