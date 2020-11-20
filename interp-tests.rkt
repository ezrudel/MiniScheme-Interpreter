#lang racket
; Ezra Rudel

(require rackunit)
(require "env.rkt" "interp.rkt")

(provide interp-tests)

(define test-env
  (env '(x y) '(10 23) init-env))

(define interp-tests
  (test-suite
   "Interpreter tests"
   (test-equal? "Number"
                (eval-exp '(lit-exp 5) empty-env)
                5)
   (test-equal? "x = 23"
                (eval-exp '(var-exp x) init-env)
                23)
   (test-equal? "y = 42"
                (eval-exp '(var-exp y) init-env)
                42)
   (test-equal? "1 - 2 = -1"
                (eval-exp '(app-exp (var-exp -)
                                    ((lit-exp 1)
                                     (lit-exp 2)))
                          init-env)
                -1)
   (test-equal? "negate 6"
                (eval-exp '(app-exp (var-exp negate)
                                    ((lit-exp 6)))
                          init-env)
                -6)
   (test-equal? "app: complex"
                (eval-exp '(app-exp (var-exp car)
                                    ((app-exp (var-exp list)
                                              ((lit-exp 3)
                                               (lit-exp 5)
                                               (lit-exp 2)))))
                          init-env)
                3)
   (test-equal? "ite: x for condition"
                (eval-exp '(ite-exp (var-exp x)
                                    (lit-exp 10)
                                    (lit-exp 20))
                          init-env)
                10)
   (test-equal? "ite: z for condition"
                (eval-exp '(ite-exp (var-exp z)
                                    (lit-exp 10)
                                    (lit-exp 20))
                          init-env)
                20)
   (test-equal? "x is eqv? to 23"
                (eval-exp '(app-exp (var-exp eqv?)
                                    ((var-exp x)
                                     (lit-exp 23)))
                          init-env)
                'True)
   (test-equal? "y is not eqv? to 3"
                (eval-exp '(app-exp (var-exp eqv?)
                                    ((var-exp y)
                                     (lit-exp 3)))
                          init-env)
                'False)
   (test-equal? "let"
                (eval-exp '(let-exp (a b)
                                    ((lit-exp 1)
                                     (lit-exp 5))
                                    (app-exp (var-exp +)
                                             ((var-exp a)
                                              (var-exp b))))
                          init-env)
                6)))
   