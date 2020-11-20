#lang racket
; Ezra Rudel

(require rackunit rackunit/text-ui rackunit/gui)
(require "env-tests.rkt" "parse-tests.rkt" "interp-tests.rkt")

(define all-tests
  (test-suite
   "All tests"
   env-tests ; 16 tests
   parse-tests
   interp-tests))

(run-tests all-tests)
