#lang plai-typed

(require "type-inference.rkt")

(test (type=? (infer-type '(+ x y))
              (numT))
      true)

(test (type=? (infer-type '  (fun (x) (+ x 3)))
              (funT (numT) (numT)))
      true)
(test (type=? (infer-type '(bif true 0 1))
              (numT))
      true)

(test (type=? (infer-type '(with ((x 3)) x))
              (numT))
      true)

(test (type=? (infer-type '(with ((x 3)) (+ 7 (+ x 3))))
              (numT))
      true)

(test (type=? (infer-type '(iszero (- 3 3)))
              (boolT))
      true)

(test (type=? (infer-type '(tempty? tempty))
              (boolT))
      true)

(test (type=? (infer-type '(- 3 4))
              (numT))
      true)

(test (type=? (infer-type '(tfirst (tcons 3 tempty)))
              (numT))
      true)

(test (type=? (infer-type '(tcons 3 tempty))
              (tlistT (numT)))
      true)

(test (type=? (infer-type '(trest (tcons true (tcons false tempty))))
              (tlistT (boolT)))
      true)

(test (type=? (infer-type '(rec (f (fun (x) (f (+ x 1))))
                             (f 10)))
              (varT 'a))
      true)

(test (type=? (infer-type '(rec (f (fun (x) (f x)))
                             f))
              (funT (varT 'a) (varT 'b)))
      true)

(test (type=? (infer-type '(iszero 3))
              (boolT))
      true)

(test (type=? (infer-type '(fun (x) (fun (y) (x y))))
              (funT (funT (varT 'a) (varT 'b))
                    (funT (varT 'a) (varT 'b))))
      true)

(test (type=? (infer-type '(fun (x) (fun (y) (y x))))
              (funT
               (varT 'a)
               (funT
                (funT (varT 'a) (varT 'b))
                (varT 'b))))
      true)


(test/exn (infer-type '(tcons a a)) "occurs check")

(test (type=? (infer-type '(rec (f (fun (x) (f x)))
                             (f x)))
              (varT 'a))
      true)

(test/exn (infer-type '(rec (f (fun (x) (f x)))
                         (f f))) "occurs check")

(test (type=? (infer-type '(fun (x) (fun (y) (fun (z) (x (y z))))))
              (funT
               (funT (varT 'a) (varT 'b))
               (funT
                (funT (varT 'c) (varT 'a))
                (funT (varT 'c) (varT 'b)))))
      true)

