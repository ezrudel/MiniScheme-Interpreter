#lang racket
; Ezra Rudel

(require rackunit rackunit/text-ui rackunit/gui)
(require "env.rkt")
(provide env-tests)

; Define an environment for testing.
(define test-env
  (env '(x y) '(1 2) empty-env))
(define test-env-2
  (env '(x z) '(5 3) test-env))

(define env-tests
  (test-suite
   "Environment tests"
   (test-true "Empty environment recognizer"
              (empty-env? empty-env))
   
   (test-true "Empty environment is an environment"
              (env? empty-env))
   
   (test-false "Empty environment is not extended"
               (extended-env? empty-env))

   (test-true "Extended environment recognizer"
              (extended-env? test-env))

   (test-true "Extended environment is an environment"
              (env? test-env))

   (test-false "Extended environment is not empty"
               (empty-env? test-env))

   (test-equal? "Symbols accessor"
                (env-syms test-env)
                '(x y))

   (test-equal? "Values accessor"
                (env-vals test-env)
                '(1 2))

   (test-equal? "Previous environment accessor"
                (env-previous test-env)
                empty-env)

   (test-equal? "Previous environment accessor with non-empty previous"
                (env-previous (env '(z) '(3) test-env))
                test-env)

   (test-exn "Empty environment has no previous"
             exn:fail?
             (λ () (env-previous empty-env)))
   (test-exn "Foo is not bound"
             exn:fail?
             (λ () (env-lookup test-env 'foo)))
   (test-exn "lookup in empty environment"
             exn:fail?
             (λ () (env-lookup empty-env 'a)))
   (test-equal? "x is 1 in test-env"
                (env-lookup test-env 'x)
                1)
   (test-equal? "x is 5 in test-env-2"
                (env-lookup test-env-2 'x)
                5)
   (test-equal? "z is 3 in test-env-2"
                (env-lookup test-env-2 'z)
                3)))
