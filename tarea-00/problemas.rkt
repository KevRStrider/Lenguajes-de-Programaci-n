#lang racket

;; 1.
(define pi 3.14)

;; 2.
(define (area-circle r)
  (* r r pi))

;; 3.
(define (circle-properties r)
  (list (area-circle r) (* 2 pi r)))

;; 4.
(define (rectangle-properties rec)
  (list (* (car rec)(cadr rec))(+ (* 2 (car rec))(* 2 (cadr rec)))))

;; 5.
(define (find-needle ls)
  (cond
    [(empty? ls) -1]
    [(equal? (car ls) 'needle) 0]
    [(equal? (cadr ls) 'needle) 1]
    [(equal? (caddr ls) 'needle) 2]
    [else -1]))

;; 6.
(define (abs x)
  (if (>= x 0) x
      (- x)))
;; 7.
(define (inclis1 ls)
  (map add1 ls))

;; 8.
(define (even? x)
  (if (= (modulo x 2) 0) #t
      #f))

 ;; 9.
(define another-add
  (lambda (n m)
    (cond
      [(zero? m) n]
      [else (add1 (another-add m (sub1 n)))])))

(provide (all-defined-out))
