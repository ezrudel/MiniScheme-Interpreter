#lang racket
; Ezra Rudel

(require "parse.rkt" "env.rkt")
(provide (all-defined-out))

(define (eval-exp tree e)
  (cond [(lit-exp? tree) (lit-exp-num tree)]
        [(var-exp? tree) (env-lookup e (var-exp-sym tree))]
        [(app-exp? tree)
         (let ([proc (eval-exp (app-exp-proc tree) e)]
               [args (map
                      (λ (arg) (eval-exp arg e))
                      (app-exp-args tree))])
           (apply-proc proc args))]
        [(ite-exp? tree)
         (if (or (eq? 'False (eval-exp (ite-exp-if tree) e))
                 (= 0 (eval-exp (ite-exp-if tree) e)))
             (eval-exp (ite-exp-else tree) e)
             (eval-exp (ite-exp-then tree) e))]
        [(let-exp? tree)
         (eval-exp (let-exp-body tree)
                   (env (let-exp-syms tree)
                        (map (λ (exp) (eval-exp exp e))
                             (let-exp-trees tree))
                        e))]
        [else (error 'eval-exp "Invalid tree: ~s" tree)]))

(define (apply-proc proc args)
  (cond [(prim-proc? proc)
         (apply-primitive-op (prim-proc-op proc) args)]
        [else (error 'apply-proc "bad procedure: ~s" proc)]))

(define (apply-primitive-op op args)
  (cond [(eq? op '+) (apply + args)]
        [(eq? op '-) (apply - args)]
        [(eq? op '*) (apply * args)]
        [(eq? op '/) (apply / args)]
        [(eq? op 'add1) (+ (first args) 1)]
        [(eq? op 'sub1) (- (first args) 1)]
        [(eq? op 'negate) (- 0 (first args))]
        [(eq? op 'list) args]
        [(eq? op 'cons) (apply cons args)]
        [(eq? op 'car) (first (first args))]
        [(eq? op 'cdr) (rest (first args))]
        [(eq? op 'number?) (if (number? (first args)) 'True 'False)]
        [(eq? op 'eqv?) (if (apply eqv? args) 'True 'False)]
        [(eq? op 'lt?) (if (apply < args) 'True 'False)]
        [(eq? op 'gt?) (if (apply > args) 'True 'False)]
        [(eq? op 'leq?) (if (apply <= args) 'True 'False)]
        [(eq? op 'geq?) (if (apply >= args) 'True 'False)]
        [(eq? op 'null?) (if (null? (first args)) 'True 'False)]
        [(eq? op 'list?) (if (list? (first args)) 'True 'False)]
        [else (error 'apply-primitive-op "Unknown primitive: ~s" op)]))

; prim-proc data type
(define (prim-proc sym)
  (list 'prim-proc sym))

(define (prim-proc? exp)
  (if (list? exp)
      (if (equal? 'prim-proc (first exp))
          #t
          #f)
      #f))

(define (prim-proc-op exp)
  (if (prim-proc? exp)
      (second exp)
      (error "~s is not a prim-proc" exp)))

(define primitive-ops '(+ - * / add1 sub1 negate
                          list cons car cdr
                          number? eqv? lt? gt? leq? geq?
                          null? list?))

(define prim-env
  (env primitive-ops
       (map prim-proc primitive-ops)
       empty-env))

(define init-env
  (env '(x y z)
       '(23 42 0)
       prim-env))