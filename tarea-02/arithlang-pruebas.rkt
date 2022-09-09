#lang plait

(require "arithlang.rkt")

(module+ test
  (test/exn (eval `()) "expresión aritmética malformada")
  (test/exn (eval `(+)) "operación aritmética malformada")
  (test/exn (eval `(-)) "operación aritmética malformada")
  (test/exn (eval `(*)) "operación aritmética malformada")
  (test/exn (eval `(n)) "operación aritmética malformada")
  (test/exn (eval `(+ 1 1 1)) "operación aritmética malformada")
  (test/exn (eval `(- 2 2 2)) "operación aritmética malformada")
  (test/exn (eval `(* 3 3 3)) "operación aritmética malformada")
  (test/exn (eval `(n 1 2)) "operación aritmética inválida")
  (test (eval `(+ 1 2)) 3)
  (test (eval `(- 3 4)) -1)
  (test (eval `(* 2 8)) 16)
  (test (eval `(+ 2 (* 2 3))) 8)
  (test (eval `(* 1 2)) 2)
  (test (eval `(* -1 2)) -2)
  (test (eval `(- 2 1)) 1)
  (test (eval `(- 1 1)) 0)
  (test (eval `(- 1 2)) -1)
  (test (eval `(- 1)) -1))
