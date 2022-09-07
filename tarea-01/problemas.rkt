#lang racket

;; Escribe aquí tus soluciones
;; 1.
(define (countdown n)
  (if (equal? n 0)
      (list 0)
      (cons n
            (countdown (- n 1)))))

;; 2.
(define (insertL a b ls)
  (if (empty? ls)
      ls
      (if (equal? a (first ls))
          (cons b (cons a (insertL a b (rest ls))))
          (cons (first ls) (insertL a b (rest ls))))))

;; 3.
(define (remv-1st a ls)
  (if (empty? ls)
      ls
      (if(equal? a (first ls))
         (rest  ls)
         (cons (first ls) (remv-1st a (rest ls))))))

;; 4.
(define (map pre ls)
  (if (empty? ls)
      ls
      (cons (pre (first ls)) (map pre (rest ls)))))

;; 5.
(define (filter pre ls)
  (if (empty? ls)
      ls
      (if (pre (first ls))
          (cons (first ls) (filter pre (rest ls)))
          (filter pre (rest ls)))))

;; 6.
(define (zip l1 l2)
  (if (or (empty? l1) (empty? l2)) 
      '()
      (cons (cons (first l1) (first l2)) (zip (rest l1) (rest l2)))))

;; 7.
(define (list-index-ofv x ls)
  'fallé-mamá)

;; 8.
(define (append ls1 ls2)
  (cond
    [(and (empty? ls1) (empty? ls2)) '()]
    [(empty? ls1) ls2]
    [(empty? ls2) ls1]
    [(cons (first ls1) (append (rest ls1) ls2))]))

;; 9.
(define (reverse ls)
  (if (empty? ls)
      ls
      (append (reverse (rest ls)) (list (first ls)))))

;; 10.
(define (repeat ls n)
  (if (equal? n 1)
      ls
      (append ls (repeat ls (sub1 n)))))
;; 11.
(define (same-lists* ls1 ls2)
  'fallé-mamá)

;; 12.
(define (binary->natural ls)
  'fallé-mamá)

;; 14.
(define (div a b)
  'falle-mama)

;; 15.
(define (append-map fn ls)
  'falle-mama)

;; 16.
(define (set-difference s1 s2)
  'falle-mama)

;; 17.
(define (foldr op a ls)
  'falle-mama)


;; 18.
(define (powerset ls)
  (if (empty? ls)
      (list ls)
      (let ([ps (powerset (rest ls))])
        (append (f (first ls) ps)
                ps))))
(define (f x ls)
  (if (empty? ls)
      ls
      (cons (cons x (first ls))
            (f x (rest ls)))))

;; 21.
(define (snowball f)
  'falle-mama)
(define (quine)
  'idk)

(provide (all-defined-out))
