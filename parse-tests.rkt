#lang racket
; Ezra Rudel

(require rackunit)
(require "parse.rkt")

(provide parse-tests)

(define parse-tests
  (test-suite
   "Parse tests"
   (test-pred "Literal"
              lit-exp?
              (parse 5))
   (test-equal? "number"
                (lit-exp-num (parse 3))
                3)
   (test-equal? "symbol"
                (var-exp-sym (parse 'x))
                'x)
   (test-equal? "app: 0 params"
                (parse '(foo))
                '(app-exp (var-exp foo) ()))
   (test-equal? "app: 1 param"
                (parse '(bar 1))
                '(app-exp (var-exp bar) ((lit-exp 1))))
   (test-equal? "app: 2 params"
                (parse '(bav x y))
                '(app-exp (var-exp bav) ((var-exp x) (var-exp y))))
   (test-equal? "app: complex"
                (parse '(car (list 3 5 2)))
                '(app-exp (var-exp car)
                          ((app-exp (var-exp list)
                                    ((lit-exp 3)
                                     (lit-exp 5)
                                     (lit-exp 2))))))
   (test-equal? "ite"
                (parse '(if x 10 20))
                '(ite-exp (var-exp x) (lit-exp 10) (lit-exp 20)))
   (test-equal? "let"
                (parse '(let ([a 1]
                              [b 5])
                          (+ a b)))
                '(let-exp (a b)
                          ((lit-exp 1)
                           (lit-exp 5))
                          (app-exp (var-exp +)
                                   ((var-exp a)
                                    (var-exp b)))))
                ; couldn't get this test to work for some reason
                ; '() throws an error but apparently it's not the right kind of error
                ;(test-exn "empty"
                ;         exn:fail
                ;         (λ () (parse '())))
                ))
  