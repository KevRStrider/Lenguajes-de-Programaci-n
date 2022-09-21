#lang racket
(define (unit-string? x)
  (and (string? x)
      (= (string-length x) 1)))

(define (unit-string-list? x)
  (or (null? x)
     (and (pair? x)
         (string?(first x))
         (= (string-length(first x))1)
         (unit-string-list? (rest x)))))

(define (explode s)
(unless(string? s)
  (error 'explode "esperaba una cadena, pero recibí: ~e" s))
(map string (string->list s)))

(define (implode ls)
  (unless (unit-string-list? ls)
    (error 'implode "esperaba una lista de cadenas unitarias, pero recibí: ~e" ls))
  (apply string-append ls))


(define (take l n)
  (if (or (<= n 0) (<= (length l) 0))
      '()
      (cons (first l) (take (rest l) (- n 1)))))

(define (drop l n)
  (if (or (= n 0) (empty? l))
      l
      (drop (rest l) (- n 1))))


(define (bundle s n)
  (cond
    [(null? s) null]
    [else (cons (implode (take s n))
                (bundle (drop s n) n))]))

;;  Consume una lista l de valores arbitrarios y un natural n. El resultado es una lista de trozos de tamaño n.
;; Cada trozo representa una sub-secuencia de elementos en l. Implementa bundle usando list->chunks.

(define (list->chunks l n)
  (cond
    [(null? l) null]
    [(= n 0) '()]
    [else (cons (take l n)
                (list->chunks (drop l n) n))]))

(define (bundle-list->chunks l n)
  (map implode (list->chunks l n)))

                
;; Toma una cadena s y un natural n. Produce una lista de trozos de cadenas de tamaño n.

(define (partition s n)
  (cond
    	[(= (string-length s) 0) '()]
	[(< (string-length s) n) (cons s '())]
	[else (cons (substring s 0 n) (partition (substring s n) n))]))
