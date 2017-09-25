#lang plai-typed

(require "calc-local-types.rkt")

(define (same-elts L1 L2) 
  (let ([all-in? (lambda (A B) (foldl (lambda (e res) (and (member e A) res)) true B))])   
    (and (all-in? L1 L2) (all-in? L2 L1))))

(test (same-elts (parse-and-calc '(open (record (a (+ 1 2)) (b 5)) :
                                        (rectype (b : number))
                                        (+ @ 6)))
                 (list 'b))
      true)

(test (same-elts (parse-and-calc '(fun (x : number) @))
                 (list 'x))
      true)

(test (same-elts (parse-and-calc '(fun (x : number)
                                       (with ((a 10))
                                             (+ @ 5))))
                 (list 'a 'x))
      true)

(test (same-elts (parse-and-calc '(fun (x : number)
                                       (fun (y : (boolean -> (rectype (t : number) (z : boolean))))
                                            (with ((a 7))
                                                  (+ @ 7)))))
                 (list 'a 'y 'x))
      true)

(test (same-elts (parse-and-calc '(open (record (a (+ 1 2)) (b @)) :
                                        (rectype (b : number))
                                        (+ 1 6)))
                 empty)
      true)

(test (same-elts (parse-and-calc '(open (record (a (+ 1 2)) (b 10)) :
                                        (rectype (a : number))
                                        (open (record (c (+ 3 (+ 4 5))) (d @)) :
                                              (rectype (c : boolean))
                                              3)))
                 (list 'a))
      true)

(test (same-elts (parse-and-calc '(open (record (a 3) (b 1)) :
                                        (rectype (a : number) (c : boolean))
                                        (fun (x : number)
                                             (open (record (d 7)) :
                                                           (rectype (e : number))
                                                   @))))
                 (list 'x 'a 'c 'e))
      true)

(test (same-elts (parse-and-calc '(with ([r (record (a (+ 1 2)) (b 5))])
                                        (num= 3 @)))
                 (list 'r))
      true)

(test (same-elts (parse-and-calc '(with ([r (record (a (+ 1 2)) (b 5))])
                                        (lookup @ a)))
                 (list 'r))
      true)

(test (same-elts (parse-and-calc '(bif true
                                       (with ((a 10)) 10)
                                       @))
                 empty)
      true)





