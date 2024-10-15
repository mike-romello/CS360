#lang racket
(require rackunit)
(require rackunit/text-ui)
(require "mceval-tests.rkt")

(provide mceval-tests)

(define mceval-tests
  (test-suite
   "Metacircular Evaluator Tests"
   basic-tests
   let-tests
   primitive-tests
   and-or-tests
   let*-tests
   force-delay-tests))
