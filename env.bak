#lang racket
; Ezra Rudel
(provide (all-defined-out))

; The empty environment is null.
(define empty-env null)

; Environment constructor.
(define (env syms vals previous-env)
  (cond [(not (list? syms)) (error 'env "syms is not a list")]
        [(not (list? vals)) (error 'env "vals is not a list")]
        [(not (env? previous-env)) (error 'env "previous-env is not an env")]
        [else (list 'env syms vals previous-env)]))

; Environment recognizers.
(define (env? e)
  (or (empty-env? e) (extended-env? e)))

(define (empty-env? e)
  (null? e))

(define (extended-env? e)
  (and (list? e)
       (not (null? e))
       (eq? (first e) 'env)))

; Environment accessors.
(define (env-syms e)
  (cond [(empty-env? e) empty]
        [(extended-env? e) (second e)]
        [else (error 'env-syms "e is not an env")]))

(define (env-vals e)
  (cond [(empty-env? e) empty]
        [(extended-env? e) (third e)]
        [else (error 'env-vals "e is not an env")]))

(define (env-previous e)
  (cond [(empty-env? e) (error 'env-previous "e has no previous env")]
        [(extended-env? e) (fourth e)]
        [else (error 'env-previous "e is not an env")]))

; helper method: returns index of x in lst,
; #f if lst does not contain x
(define (index x lst)
  (cond [(empty? lst) #f]
        [(equal? x (first lst)) 0]
        [else (if (index x (rest lst))
                  (add1 (index x (rest lst))) #f)]))

; helper method: returns nth thing in lst
(define (get n lst)
  (if (= n 0) (first lst) (get (sub1 n) (rest lst))))

; returns first binding for symbol sym in environment e
(define (env-lookup e sym)
  (cond [(empty-env? e) (error 'env-lookup "No binding for ~s" sym)]
        [(index sym (env-syms e))
         (get (index sym (env-syms e)) (env-vals e))]
        [else (env-lookup (env-previous e) sym)]))



