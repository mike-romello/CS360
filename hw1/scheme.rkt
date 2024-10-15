;;
;; DO NOT REMOVE THESE TWO LINES
;;
#lang racket
(provide (all-defined-out))

;;
;; Problem 1
;;
;; Number Integer -> Number
;;
;; Compute the falling factorial x to the n falling
;;

(define (falling x n)
  (if (= n 0)
      1
      (* x (falling (- x 1) (- n 1)))))
;;
;; Problem 2
;;
;; Integer Integer -> Integer
;;
;; Compute the Stirling number of the second kind, n subset k
;;

(define (S n k)
  (cond
    [(equal? n k) 1]
    [(equal? n 0) 0]
    [(equal? k 0) 0]
    [else (+ (* k (S (- n 1) k)) (S (- n 1) (- k 1)))]))

;;
;; Problem 3
;;
;; List List -> List
;;
;; Produce a list of pairs where the first element of each pair is take from
;; the first arguments xs and the second element of each pair is taken from the
;; second argument, ys.
;;

(define (zip xs ys)
  (cond
    [(null? xs) null]
    [(null? ys) null]
    [(not (list? xs)) null]
    [(not (list? ys)) null]
    [else (cons (cons (first xs) (first ys)) (zip (rest xs) (rest ys)))]))

;;
;; Problem 4
;;
;; (Number -> Number) -> (Number -> Number)
;;
;; Compute the forward difference operator for f.
;;

(define (delta f)
  (lambda (x) (- (f (+ x 1)) (f x))))

;;
;; Problem 5
;;
;; (Any -> Number) -> List -> Any
;;
;; Compute the element x in xs for which (f x) is greatest.
;;

(define (argmax f xs)
  (define (argmax-help f xs max)
    (if (null? xs)
        max
        (if (> (f (first xs)) (f max))
            (argmax-help f (rest xs) (first xs))
            (argmax-help f (rest xs) max))))
  (argmax-help f (rest xs) (first xs)))