#lang plai-typed

(require "calc-locals-untyped.rkt")

(define (same-elts L1 L2) 
  (let ([all-in? (lambda (A B) (foldl (lambda (e res) (and (member e A) res)) true B))])   
    (and (all-in? L1 L2) (all-in? L2 L1))))

(test (same-elts (parse-and-calc '(fun (x)
                                       (with ((b 10))
                                             (open (record (t 10) (u 10))
                                                   (with ((a @)) 10)))))
                 (list 'x 'b 't 'u))
      true)

(test (same-elts (parse-and-calc '(bif true
                                       (with ((a 10)) 10)
                                       @))
                 empty)
      true)

(test (same-elts (parse-and-calc '(with ((d 10))
                                        (with ((r (record (a (+ 1 2)) (b 5))))
                                              (extend r c @))))
                 
                 (list 'd 'r))
      true)

#|
(test (parse-and-calc '(with ((d 10))
                             (with ((r (open (record (a (+ 1 2)) (b 5))
                                             (+ 1 a)))
                                   (extend r c @)))))
                           
      (list 'd 'c))
|#

(test (same-elts (parse-and-calc '(with ([r (record (a (+ 1 2)) (b 5))])
                                        (lookup @ a)))
                 (list 'r))
      true)

(test (same-elts (parse-and-calc '(with ([r (record (a (+ 1 2)) (b 5))])
                                        (num= 3 @)))
                 (list 'r))
      true)

;; test
;(parse-and-calc '(open (record (a : 5) (b : 6))
;      (rectype (a : number)))
; -> (list 'a)

(test (same-elts (parse-and-calc '(with ((a 3)) (record (b 4) (c 5)) @))
                 (list 'a))
      true)

; error
#|
(with ((r (bif true
               (record (x 5))
               (record (y 5)))))
      (open r  @))

(parse-and-calc '(open (extend (record (a 10)) b 15) @))

(parse-and-calc '(open (record (a (record (a 3))))
                       (open a @)))


|#

(test (same-elts (parse-and-calc '(with ((a 10))
                                        (record (c (+ 1 3)) (b @))
                                        10))
                 (list 'a))
      true)

(test (same-elts (parse-and-calc '(open (record (a 0) (b 1) (c 4))
                                        (with ([o 6])
                                              (num= @ 2))))
                 (list 'o 'a 'b 'c))
      true)

(test (same-elts (parse-and-calc '(open (record (a 0) (b 1) (c 4))
                                        (open (record (e (+ 1 @)))
                                              (+ 1 2))))
                 (list 'a 'b 'c))
      true)

(test (same-elts (parse-and-calc '(fun (y) (open (record (a @)) 3)))
                 (list 'y))
      true)










