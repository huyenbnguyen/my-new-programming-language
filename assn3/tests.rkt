#lang plai-typed

(require "interp.rkt")

; test that box is creating a box
(test (boxV? (run '(box 1)))
      true)

; test that boxC is returning the updated store
(test (run '(with ([b (box 1)])
                  (unbox b)))
      (numV 1))

; test that boxC is allocating new address for each new box
(test (run '(with ([b1 (box 1)] [b2 (box 2)] [b3 (box 3)])
                  (unbox b2)))
      (numV 2))

; test that unboxC is fetching from the updated store
(test (run '(with ([b (box 0)])
                  (unbox (seq (setbox b 1)
                              b))))
      (numV 1))

; test that unboxC is returning the updated store
(test (run '(with ([b (box 0)])
                  (+ (unbox (seq (setbox b 1)
                                 b))
                     (unbox b))))
      (numV 2))

; test that unboxC is throwing an error
; when the box doesn't exist
(test/exn (run '(with ([b (box 1)])
                      (unbox c)))
          "unbound")


; test that setboxC is interpreting the value
; using the store returned from interpreting
; the box
(test (run '(with ([b (box 1)])
                  (seq (setbox b (+ 1 (unbox b)))
                       (unbox b))))
      (numV 2))

; test that setboxC is returning the value
; returned from interpreting the value
;(test (run '(v*s-v (setbox (box 1) 2)))
;      (numV 2))

; test that setboxC is returning the updated store
(test (run '(with ([b (box 1)])
                  (with ([c (box 2)])
                             (seq (setbox b 3)
                                  (unbox b)))))
      (numV 3))
                   
; test that setboxC is throwing an error
; when the box doesn't exist
(test/exn (run '(with ([b (box 1)])
                      (setbox c 5)))
          "unbound")

; test that seqC is using the updated store returned from
; interpreting b1 to interpret b2
(test (run '(with ([b (box 1)])
                  (seq (setbox b 2)
                       (unbox b))))
      (numV 2))

;--------------------------------------------------------------------------

; test that seqC is interpreting b1 before
; interpreting b2
(test (run '(with ([b (box 1)] [c (box 2)])
                  (seq (setbox b 2)
                       (+ (unbox b) (unbox c)))))
      (numV 4))

; test that plusC is using the updated store
; returned from interpreting l when
; interpreting r
(test (run '(with ([b (box 1)])
                  (+ (seq (setbox b 2)
                          (unbox b))
                     (+ 1 (unbox b)))))
      (numV 5))

; test that plusC is returning the updated store
; from interpreting r
(test (run '(with ([b (box 1)])
                  (seq (+ 1
                          (seq (setbox b 2)
                               (unbox b)))
                       (unbox b))))
      (numV 2))

; test that multC is using the updated store
; returned from interpreting l when
; interpreting r
(test (run '(with ([b (box 1)])
                  (* (seq (setbox b 2)
                          (unbox b))
                     (* 2 (unbox b)))))
      (numV 8))

; test that multC is returning the updated store
; from interpreting r
(test (run '(with ([b (box 1)])
                  (seq (* 2
                          (seq (setbox b 2)
                               (unbox b)))
                       (unbox b))))
      (numV 2))

; test that setC is returning the updated store
(test (run '(with ([a 1])
                  (seq (set a 2)
                       a)))
      (numV 2))

; TYPE TESTING
; try to use unbox on a variable
(test/exn (run '(with ([a 1])
                      (seq (set a 2)
                           (unbox a))))
          "type")
; try to use setbox on a variable
(test/exn (run '(with ([a 1])
                      (seq (set a 2)
                           (setbox a 3))))
          "type")
; change a box to a variable using set
; then get the value of the variable
(test (run '(with ([b (box 1)])
                  (seq (set b 2)
                       b)))
      (numV 2))
; change a variable to a box
; then use unbox on the box
(test (run '(with ([b 1])
                  (seq (set b (box 2))
                       (unbox b))))
      (numV 2))
; assign the box to a function
(test (run '(with ([b (box (fun (n) n))])
                  ((unbox b) 2)))
      (numV 2))
; try to add 2 boxes together
(test/exn (run '(+ (box 1) (box 2)))
          "type")
; try to multiply 2 boxes together
(test/exn (run '(* (box 1) (box 2)))
          "type")


; MULTIPLE TESTING
; having the same identifier multiple times
; in the same with
(test/exn (run '(with ([b (box 1)] [b 2])
                      b))
          "multiple")
(test/exn (run '(with ([a 1] [a 2])
                      a))
          "multiple")
(test/exn (run '(with ([a (box 1)] [a 1])
                      a))
          "multiple")

; test that unbox is fetching the most recent
; value of the box
(test (run '(with ([b (box 1)])
                  (with ([b (box 2)])
                        (unbox b))))
      (numV 2))
; test that the program is fetching the most recent
; value of the variable
(test (run '(with ([b 1])
                  (with ([b 2])
                        b)))
      (numV 2))

; PASS BY VALUE TESTING
; test pass by value with function
; make sure that appC is allocating
; new address for the parameter
(test (run '(with ([x 3]
                   [f (fun (y) (+ 1 y))])
                  (seq (f x)
                       x)))
      (numV 3))

; test parameter passing with variables
(test (run '(with ([y 6])
                  (with ([x y])
                        (seq (set y 4)
                             x))))
      (numV 6))
; test parameter passing with boxes
(test (run '(with ([x (box 6)])
                  (with ([y x])
                        (seq (setbox y 4)
                             (unbox x)))))
      (numV 4))

; UNBOUND TESTING
; try using set on a variable
; that hasn't been defined
(test/exn (run '(set a 3))
          "unbound")















