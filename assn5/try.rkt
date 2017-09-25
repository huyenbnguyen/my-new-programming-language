#lang racket

(define addclass
  (λ (w)
    (let ([private-w w] [v 6] [q 5])
      (λ (msg)
        (case (msg)
          [(add) (λ (num) (set! private-w (+ num private-w)))]
          [(subpub) (λ () (set! private-w (- private-w v)))])))))



; convert this to ExprC code
(define addclass
  (lamC (cons 'w empty)
        (desugar (withS ([p-c w] [v 6] [q 5])
          (lamC (msg) ; must be an obj
                (varcarseC 'msg
                           (list (optC 'add (fun (num) (setC p-c (+ num p-c))))
                                 (optC 'subpub (fun () (setC p-c (- p-c v))))))))))))))
