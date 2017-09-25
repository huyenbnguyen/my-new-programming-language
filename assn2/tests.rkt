#lang plai-typed
(require "interp.rkt")

; this is a sample test suite

(define p1 '(+ 3 4))

(test (run p1) (numV 7))
(test/exn (run '(+ x 4)) "unbound")