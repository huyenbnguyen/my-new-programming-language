#lang plai-typed

; Assignment operator in (with ([x y]]) ...) is done by reference
; because I expected the value of x to be unchanged when y is changed to 3
; but the value of x is changed to be the new value of y
; Another question is how did the program record changes made to the variable y?
; I think the language must use a Store because the Store enables memory addresses
; to be passed around
; The Env doesn't allow this
; Another thing I got out of this test case is the implementation of set uses
; the Store to record changes
(test (run '(with ([y 5])
                  (with ([x y])
                        (seq (set y 3)                          
                             x))))
      (numV 3))

; Passing a variable to a function is done by reference
; the value of y is modified in this case
(test (run '(with ([f (fun (x) (set x 3))])
                  (with ([y 5])
                        (seq
                         (f y)
                         y))))
      (numV 3))

; As I stated in the first test case, the assignment operator in with
; is done by reference.
; So any changes being made in box x will also be reflected in box y
; But when we use setbox on box x, and try to unbox box y,
; the content in box y is changed, so setbox does record changes here
(test (run '(with ([x (box 6)])
                  (with ([y x])
                        (seq (setbox x 4)
                             (unbox y)))))
      (numV 4))

; the language implements static binding
; because it doesn't use the definition of g
; provided in the innermost with
(test (run '(with ([x 10]
                   [g (fun (x) (+ x 5))])
                  (with ([f (fun (y) (+ y (g 5)))])
                        (with ([g (fun (x) (+ x 100))]
                               [x 20])
                              (f 5)))))  
      (numV 15))

; the language implements eager evaluation because
; in this test case, f doesn't need to use the parameter a
; anywhere in its function body, but it does
; interprete the expression being passed in
; as a result, the content of box b is changed
(test (run '(with ([b (box 1)])
                  (with ([f (fun (a) (unbox b))])
                        (f (setbox b 2)))))
      (numV 2))

; the language does use Env to get most recent value
; of the identifier (after taking closure into consideration)
; we see in this test case, the result 7 comes from adding
; the new value of a (which is 4), and the content of box b
; (which is 3)
(test (run '(with ([a 2] [b (box 1)])
                  (with ([c 1])
                        (with ([a 4] [b (box 3)])
                              (+ (+ a c) (unbox b))))))
      (numV 8))

; + and * operations pass the updated Store to the left operand
; because after changing the value of x to 2, this value is used
; throughout the computation
(test (run '(with ([x 1])
                  (*
                   (+ (set x 2)
                      x)
                   x)))
      (numV 8))

; set returns the new value being set to the variable
; in this test case, it is important to not think that
; the value of x is changed after being passed into
; the function f.
; The reason why the value of x changes is because
; set is used
(test (run '(with ([x 10]
                   [f (fun (y) (set y 5))])
                  (seq (set x (f x))
                       x)))
      (numV 5))

; memory address is being incremented when:
; - new identifier is defined
(test (run '(with ([a 1] [b (box 1)])
                  b))
      (boxV 2))


           