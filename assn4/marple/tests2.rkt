
#lang plai-typed

;---------------------------------------------------------
; CALL BY VALUE VS. CALL BY REFERENCE
; ATTENTION! Pamameter passing is call by reference 
(test (run '(with ([y 6])
                  (with ([x y])
                        (seq (set y 4)
                             x))))
      (numV 4))

; test that program DOES create a fresh memory location
; in the store on each function call for the parameter
(test (run '(with ([f (fun (x) (set x 3))])
                  (with ([y 5])
                        (seq
                         (f y)
                         y))))
      (numV 5))


; ATTENTION! Box passing is call by value, AND
; looks like not using a Store
; x -> [100]
; [100] -> (box 6)
; 
(test (run '(with ([x (box 6)])
                  (with ([y x])
                        (seq (setbox y 4)
                             (unbox y)))))
      (numV 6))

(test (run '(with ([x 1])
                  (*
                   (+ (set x 2)
                      x)
                   x)))
      (numV 8))

; EAGER APPLICATION VS. LAZY APPLICATION
; the program applies eager application
(test (run '(with ([b (box 1)])
                  (with ([f (fun (a) (unbox b))])
                        (f (setbox b 2)))))
      (numV 1))

(test (run '(with ([b 1])
                  (with ([f (fun (a) b)])
                        (f (set b 2)))))
      (numV 1))

; STATIC BINDING VS. DYNAMIC BINDING
; the program applies static binding
(test (run '(with ([x 10]
                   [g (fun (x) (+ x 5))])
                  (with ([f (fun (y) (+ y (g 5)))])
                        (with ([g (fun (x) (+ x 100))]
                               [x 20])
                              (f 5)))))  
      (numV 15))



; BOX 
; unboxC is NOT fetching from the updated store
(test (run '(with ([b (box 0)])
                  (unbox (seq (setbox b 1)
                              b))))
      (numV 0))

; test that setboxC is NOT interpreting the value
; using the store returned from interpreting
; the box
(test (run '(with ([b (box 1)])
                  (seq (setbox b (+ 1 (unbox b)))
                       (unbox b))))
      (numV 1))

; test that plusC is NOT returning the updated store
; from interpreting r
(test (run '(with ([b (box 1)])
                  (seq (+ 1
                          (seq (setbox b 2)
                               (unbox b)))
                       (unbox b))))
      (numV 2))



; MULTIPLE TESTING
; the program does raise an error when
; there's multiple bindings
(test/exn (run '(with ([b (box 1)] [b 2])
                      b))
          "multiple")

; test that unbox is fetching the most recent
; value of the box from the Environment
(test (run '(with ([b (box 1)])
                  (with ([b (box 2)])
                        (unbox b))))
      (numV 2))

; test that multC is NOT using the updated store
; returned from interpreting l when
; interpreting r
(test (run '(with ([b (box 1)])
                  (* (seq (setbox b 2)
                          (unbox b))
                     (* 2 (unbox b)))))
      (numV 2))



(test (run '(with ([b 6])
                  (seq (set b 7)
                       b)))
      (numV 7))





(test/exn (run '(with ([f (fun (x)
                               (fun (y)
                                    (+ x y)))])
                      ((f 3) (+ x 4))))
          "unbound")


(test (run '(with ([f (fun (x)
                           (fun (y)
                                (+ x y)))])
                  (with ([x 5])
                        ((f 3) x))))      
      (numV 8))


;---------
; OTHER TESTS
#|
(test (run '(with ([x 3])
                  (+ (with ([x 5])
                           x)
                     x)))
      (numV 8))

(test (run '(with ([x 3])
                  (+ x
                     (seq (set x 5)
                          x)
                     )))
      (numV 8))

(test (run '(with ([x 3])
                  (with ([y 5])
                        (with ([x 10])
                              (+ x y)))))
      (numV 15))


(test (run '(with ([x 3])
                  (with ([f (fun (y) (+ x y))])
                        (with ([x 5])
                              (f 10)))))
      (numV 13))

; GOT (numV 9)
(test (run '(with ([x 0])
                  (with ([f (fun (y)
                                 (with ([x 5])
                                       (set x y)))]
                         [a 9])
                        (seq (f a)
                             (+ a x)))))
      (numV 18))

(test (run '(with ([x 3])
                  (with ([x 1])
                        (with ([f (fun (y) (+ x y))])
                              (with ([x 5])
                                    (f 10))))))
      (numV 11))

(test (run '(with ([x 3])
                  (+ (seq (set x 5)
                          x)
                     x)))
      (numV 10))

(test/exn (run '(with ([x 3])
                      (with ([f (fun (y) (+ z (+ y z)))])
                            (with ([z 5])
                                  (f 10)))))
          "unbound")
           

(test (run '(with ([g (fun (z)
                           (seq (set z 6)
                                0))])
                  (with ([f (fun (y)
                                 (+ (g y) y))])
                        (f 1))))
      (numV 1))

(test (run '(with ([x 2])
                  (with ([y x])
                        (seq (set y 8)
                             (+ x y)))))
      (numV 10))

; GOT (numV 10)
(test (run '(with ([x 7])
                  (with ([y x])
                        (seq (set x 5)
                             (+ x y)))))
      (numV 12))

(with ([x 7])
                  (with ([y x])
                        (seq (set x 5)
                             y)))

; test that multC is NOT using the updated store
; returned from interpreting l when
; interpreting r
(test (run '(with ([b (box 1)])
                  (* (seq (setbox b 2)
                          (unbox b))
                     (* 2 (unbox b)))))
      (numV 2))

; test that program DOES create a fresh memory location
; in the store on each function call for the parameter
(test (run '(with ([f (fun (x) (set x 3))])
                  (with ([y 5])
                        (seq
                         (f y)
                         y))))
      (numV 5))


(test (run '(with ([x 5] [f (fun (x) (set x 3))])
                 
                        (seq
                         (f x)
                         x)))
      (numV 5))


(test (run '(with ([f (fun (x) (setbox x 3))])
                  (with ([y (box 5)])
                        (seq
                         (f y)
                         (unbox y)))))
      (numV 5))

|#

(test (run '(with ([x 10]
                   [f (fun (y) (set y 5))])
                  (seq (set x (f x))
                       x)))
      (numV 5))








