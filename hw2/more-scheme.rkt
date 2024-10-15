;;
;; DO NOT REMOVE THESE TWO LINES
;;
#lang racket
(provide (all-defined-out))

;;
;; Problem 1
;;
;; A stream-of-pairs is one of
;; - the empty stream
;; - stream-cons of a pair and a stream
;;
;; (Any -> Any) Stream -> Stream-of-pairs
;;
;; Given a function f and a stream s, return a stream consisting of each element
;; x of the stream s paired with (f x).
;;

(define (stream-pair-with f s)
  (if (stream-empty? s)
      s
      (stream-cons (cons (stream-first s) (f (stream-first s))) (stream-pair-with f (stream-rest s)))))

;;
;; Problem 2
;;
;; (Any -> Any) Any -> Stream
;;
;; Given a function and a value x, return am infinite stream consisting of
;; repeated applications of f to x. The elements of the stream are x, (f x),
;; (f (f x)), ...
;;

(define (stream-iterate f x)
  (stream-cons x (stream-iterate f (f x))))
;;
;; Problem 3
;;
;; Stream Stream -> Stream
;;
;; zip for streams. Return a stream whose elements are pairs where the first
;; item in the pair is taken from xs and the second item in the pair is taken
;; from ys.
;;

(define (stream-zip xs ys)
  (if (or (stream-empty? xs) (stream-empty? ys))
      empty-stream
      (stream-cons (cons (stream-first xs) (stream-first ys)) (stream-zip (stream-rest xs) (stream-rest ys)))))

;;
;; Problem 4
;;
;; Stream Stream -> Stream
;;
;; Return a stream whose elements are pairs where the first item in the pair is
;; taken from xs and the second item in the pair is taken from ys. If xs or ys
;; is finite and runs out of element, start back at the beginning of the stream.
;;
;; Note that xs and ys may or may not be infinite, and they may or may not have
;; the same length if they are finite.
;; 

(define (cycle-streams xs ys)
  
  (define (cycle-streams-help s org)
    (if (stream-empty? s)
        (cycle-streams-help org org)
        (stream-cons (stream-first s) (cycle-streams-help (stream-rest s) org))))
  
  (stream-zip (cycle-streams-help xs xs) (cycle-streams-help ys ys)))

;;
;; Problem 5
;;
;; (Any -> Any) Any -> Pair
;;
;; Given a function f and a value x, return a pair whose first element is (f x)
;; and whose second element is the number of times apply-count has been called.
;;

(define apply-count
  (let ((counter 0))
    (lambda (f x)
      (set! counter (+ 1 counter))
      (cons (f x) counter))))
