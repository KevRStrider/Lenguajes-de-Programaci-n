#lang plait

(define-type ArithC
  [numC (num : Number)]
  [plusC (l : ArithC) (r : ArithC)]
  [multC (l : ArithC) (r : ArithC)])

(define-type ArithS
  [numS (num : Number)]
  [plusS (l : ArithS) (r : ArithS)]
  [multS (l : ArithS) (r : ArithS)]
  [minusS (l : ArithS) (r : ArithS)]
  [negS (c : ArithS)])



(define (parse [s : S-Exp]) : ArithS
  (cond [(s-exp-number? s) (numS (s-exp->number s))]
        [(and (s-exp-list? s) (not (equal? s `())))
         (let ([ls (s-exp->list s)])
           (cond
             [(not (s-exp-symbol? (first ls))) (error 'parse "No es expresion valida, ya que no es simbolo valido")]
             [(equal? (length ls) 2)
              (case (s-exp->symbol (first ls))
                [(-) (negS (parse (second ls)))])]
             [(equal? (length ls) 3)
              (case (s-exp->symbol (first ls))
                [(+) (plusS (parse (second ls)) (parse (third ls)))]
                [(*) (multS (parse (second ls)) (parse (third ls)))]
                [(-) (minusS (parse (second ls)) (parse (third ls)))]
                [else (error 'parse "operación aritmética inválida")])]
              [else (error 'parse "operación aritmética malformada")]))]
        [else (error 'parse "expresión aritmética malformada")]))


(define (desugar [s : ArithS]) : ArithC
  (type-case ArithS s
    [(numS n) (numC n)]
    [(plusS l r) (plusC (desugar l) (desugar r))]
    [(multS l r) (multC (desugar l) (desugar r))]
    [(minusS l r) (plusC (desugar l) (multC (numC -1) (desugar r)))]
    [(negS c) (multC (numC -1) (desugar c))]))

(define (interp [s : ArithC]) : Number
  (type-case ArithC s
    [(numC n) n]
    [(plusC l r) (+ (interp l) (interp r))]
    [(multC l r) (* (interp l) (interp r))]))

(define (eval [input : S-Exp]) : Number
  (interp (desugar (parse input))))
