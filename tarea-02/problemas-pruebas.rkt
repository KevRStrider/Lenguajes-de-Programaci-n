#lang racket

(require rackunit
         rackunit/text-ui
         "problemas.rkt")

(define-test-suite pruebas
  (test-case "bundle"
             (check-equal? (bundle (explode "abcdef") 1)
                           (list  "a" "b" "c" "d" "e" "f"))
             (check-equal? (bundle (explode "abcdef") 2)
                           (list "ab" "cd" "ef"))
             (check-equal? (bundle (explode "abcdefgh") 3)
                           (list) "abc" "def")
             (check-equal? (bundle '() 5)
                           '())
             (check-equal? (bundle '("a" "b" "c") 4)
                           (list "abc"))
             (check-exn exn:fail? (thunk (bundle '("") 3)))
             (check-exn exn:fail? (thunk (bundle (explode "abcdefgh") 0))))

  (test-case "take"
             (check-equal? (take '(1 2 3 4 5 6) 3)
                           '(1 2 3))
             (check-equal? (take '(1 2 3 4 5 6) 0)
                           '())
             (check-equal? (take '() 3)
                           '())
             (check-equal? (take '(1 2) 3)
                           '(1 2)))

(test-case "drop"
             (check-equal? (drop '(1 2 3 4 5 6) 3)
                           '(4 5 6))
             (check-equal? (drop '(1 2 3 4 5 6) 0)
                           '(1 2 3 4 5 6))
             (check-equal? (drop '() 3)
                           '())
             (check-equal? (drop '(1 2) 3)
                           '()))

  (test-case "list->chunks"
             (check-equal? (list->chunks '() 0) '())
             (check-equal? (list->chunks '(2 4 75 9 0 9) 0) '())
             (check-equal? (list->chunks '(2 4 75 9 0 9) 3) '((2 4 75) (9 0 9)))
             (check-equal? (list->chunks '(2 4 75 9 0 9) 4) '((2 4 75 9) (0 9))))
  
  (test-case "bundle-list->chunks"
             (check-equal? (bundle-list->chunks (explode "abcdefg") 3)
                           (list "abc" "def" "g"))
             (check-equal? (bundle-list->chunks (explode "abcdefgh") 2)
                           (list "ab" "cd" "ef" "gh"))
             (check-equal? (bundle-list->chunks (explode "abcdefgh") 1)
                           (list "a" "b" "c" "d" "e" "f" "g" "h"))
             (check-equal? (bundle-list->chunks '() 2)
                           '())
             (check-equal? (bundle-list->chunks '("a" "b") 3)
                           (list "ab")))
  
  (test-case "partition"
             (check-equal? (partition "" 3) '())
             (check-equal? (partition "holaaa" 0) '())
             (check-equal? (partition "holaaa" 2) '("ho" "la" "aa"))
             (check-equal? (partition ":)" 3) '(":)"))))
  