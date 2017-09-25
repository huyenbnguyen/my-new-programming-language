#lang plai-typed

(require "interp.rkt")

; test suites

; test 1: addition and multiplication
(define p1 '(* 3 (+ 3 4)))
(test (run p1) (numV 21))

; test 2: unbound identifiers
(test/exn (run '(+ x 4)) "unbound")

; test 3: same identifiers multiple times
(define p2 '(with ([x 3] [x 4])
                  (numV 2)))
(test/exn (run p2) "multiple")

; test 4: scoping issue - 1 variable
(define p3 '(with ([x 3])
                  (with ([x 7])
                        (+ x 2))))
(test (run p3) (numV 9))

; test 5: type error
(test/exn (run '(+ 3
                   (fun (_) (numC 2))))
          "type")

; test 6: test a zero argument function
(test (run '((fun () (+ 5 4)))) (numV 9))


; test 7: test if the language is able to
; get the value of the identifier from the environment
(test (run/env '(+ x  3) (cons (bind 'x (numV 3)) empty)) (numV 6))

; test 8: unbound identifier due to scope
(define p5 '(with ([x 3] [y x])
                  (+ x y)))
(test/exn (run p5) "unbound")

; test 9: scoping issue - 2 variables
(define p6 '(with ([x 3])
                  (with ([x 6] [y x])
                        (+ x y))))
(test (run p6) (numV 9))

; test 10: define a funC using with and apply it in the body
(define p7 '(with ([add1 (fun (n) (+ n 1))])
                  (add1 5)))
(test (run p7) (numV 6))

; test 11: test appC with function that has no identifiers
; and an empty env
(define p8 '(+ 10
               (with ([constfunc (fun (_) 5)])
                     (constfunc 10))))

(test/exn (run p8) "unbound")

; test 12: condition of if0C is 1,
; then should always return the true expression
(define p9 '(if0 0
                 1
                 2))                 
(test (run p9) (numV 1))

; test 13: assign an identifier to itself using with
(define p10 '(with ([x 3])
                   (with ([x x])
                         x)))
(test (run p10) (numV 3))

; test 14: grammar error when passing a funC as
; the condition to if0C
(test/exn (run '(if0 (fun (_) 5)
                     1
                     2))
          "type error")


; test 15: test that if0 still works
; on condition given as a function
(test (run '(if0 ((fun (x) x) 1) 999 100))
      (numV 100))

; test 16: define a function but not use it
(test (run '(fun (x) (+ x 3)))
      (closV (cons 'x empty)
             (plusC (idC 'x) (numC 3))
             mt-env))

; test 17: test nested with
(test (run '(with ([x 5])
                  (with ([y 6]) (+ x y))))
      (numV 11))

; test 18: test function with multiple arguments
(test (run '((fun (x y) (+ x y)) 5 3)) (numV 8))

; test 19: static binding test
(test (run '(with ([x 10]
                   [g (fun (x) (+ x 5))])
                  (with ([f (fun (y) (+ y (g 5)))])
                        (with ([g (fun (x) (+ x 100))]
                               [x 20])
                              (f 5)))))  
      (numV 15))

; test 20: Test that a function may call another function with
; the same variable name
(test (run '(+ (with ([x 5] [f (fun (x) (* 2 x))]
                            [g (fun (x) (* x x))])
                     (f (g 5)))
               5))
      (numV 55))



(desugar (parse '(with ([constfunc (fun (_) (numC 5))])
                     (constfunc 10))))

(test (run '(with ([a 2])
                  a))
      (numV 2))

#|
(desugar (parse '(with ([x 5] [f (fun (x) (set x 3))])
                 
                        (seq
                         (f x)
                         x))))
|#

(test (run '(with ([b (box 1)])
                  (with ([f (a) (unbox b)])
                        (f (setbox b 2)))))
      (numV 1))

(desugar (parse '(with ([x 10])
                       x)))


(with ([f (fun (a) 3)])
      (f 32))



