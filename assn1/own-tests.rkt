#lang plai-typed
(require "interp.rkt")
;1 Basic addition
(test (run '(+ 3 4)) (numV 7))

;2 nesting and multiplication
(test (run '(+ 3 (* 10 3))) (numV 90))

;3 subtraction
(test (run '(- (+ 3 (* 10 3)) 80)) (numV 10))

;4 with and function
;(test (run '(with ['double (fun (x) (+ x x))]
;                  (double 5))) 10)

;5 test shadowing
(test (run '(with ['d (fun (x) (+ x x))]
                  (with ['d (fun (x) (x))]
                        (d 5)))) (numV 5))

;6 test function scope
(test/exn (run '(f (with ['f (fun (x) (x))]
                         (+ 1 1)))) "unbound")

;7 test variable scope
(test/exn (run '(+ x (with ['f (fun (x) (x))]
                         (f 1)))) "unbound")

;8 test variable scope
(test/exn (run '(+ 1 (with ['f (fun (x) (x))]
                         (f x)))) "unbound")

;9 test if0
(test (run '(if0 0 1 2)) (numV 1))

;10 test if0
(test (run '(if0 1 1 2)) (numV 2))

;11 test scoping
(test/exn (run '(with ['f (fun ('x 'y) (* x y))]
                  ['n (fun ('x) (+ x y))]
                  (n 1))) "unbound")

;12 test scoping
(test/exn (run '(with ['f (fun ('x 'y) (* x y))]
                  ['n (fun ('x 'y) (f x y))]
                  (n 1))) "unbound")

;13 test lambda
(test (run '((fun ('x 'y) (+ x (* x y))) 2 3)) (numV 8))

;14 test scope
(test/exn (run '(with ['x 3]
                  ['y x])) "unbound")

;15 test scope
(test (run '(with ['x 3]
                  (with ['f (fun ('x) (x))]
                        (f 4)))) (numV 4))

;16 test scope
(test (run '(with ['x 3]
                  (with ['f (fun ('y) (+ x y))]
                        (with ['x 4]
                              (f 3))))) (numV 7))