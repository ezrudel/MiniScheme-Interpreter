#lang racket
; Ezra Rudel

(provide (all-defined-out))

; lit-exp data type
(define (lit-exp n)
  (list 'lit-exp n))

(define (lit-exp? exp)
  (if (list? exp)
      (if (equal? 'lit-exp (first exp))
          #t
          #f)
      #f))

(define (lit-exp-num exp)
  (if (lit-exp? exp)
      (second exp)
      (error "~s is not a lit-exp" exp)))

; var-exp data type
(define (var-exp s)
  (list 'var-exp s))

(define (var-exp? exp)
  (if (list? exp)
      (if (equal? 'var-exp (first exp))
          #t
          #f)
      #f))

(define (var-exp-sym exp)
  (if (var-exp? exp)
      (second exp)
      (error "~s is not a var-exp" exp)))

; app-exp data type
(define (app-exp proc args)
  (list 'app-exp proc args))

(define (app-exp? exp)
  (if (list? exp)
      (if (equal? 'app-exp (first exp))
          #t
          #f)
      #f))

(define (app-exp-proc exp)
  (if (app-exp? exp)
      (second exp)
      (error "~s is not a app-exp" exp)))

(define (app-exp-args exp)
  (if (app-exp? exp)
      (third exp)
      (error "~s is not a app-exp" exp)))

; ite-exp data type
(define (ite-exp if then else)
  (list 'ite-exp if then else))

(define (ite-exp? exp)
  (if (list? exp)
      (if (equal? 'ite-exp (first exp))
          #t
          #f)
      #f))

(define (ite-exp-if exp)
  (if (ite-exp? exp)
      (second exp)
      (error "~s is not a ite-exp" exp)))

(define (ite-exp-then exp)
  (if (ite-exp? exp)
      (third exp)
      (error "~s is not a ite-exp" exp)))

(define (ite-exp-else exp)
  (if (ite-exp? exp)
      (fourth exp)
      (error "~s is not a ite-exp" exp)))

; let-exp data type
(define (let-exp syms trees body)
  (list 'let-exp syms trees body))

(define (let-exp? exp)
  (if (list? exp)
      (if (equal? 'let-exp (first exp))
          #t
          #f)
      #f))

(define (let-exp-syms exp)
  (if (let-exp? exp)
      (second exp)
      (error "~s is not a let-exp" exp)))

(define (let-exp-trees exp)
  (if (let-exp? exp)
      (third exp)
      (error "~s is not a let-exp" exp)))

(define (let-exp-body exp)
  (if (let-exp? exp)
      (fourth exp)
      (error "~s is not a let-exp" exp)))


; parser
(define (parse input)
  (define (parse-error)
    (error 'parse "Invalid syntax ~s" input))
  (cond [(number? input) (lit-exp input)]
        [(symbol? input) (var-exp input)]
        [(list? input)
         (cond [(empty? input) (parse-error)]
               [(eq? (first input) 'if)
                (if (= (length input) 4)
                    (ite-exp
                     (parse (second input))
                     (parse (third input))
                     (parse (fourth input)))
                    (error 'parse "Invalid syntax ~s" input))]
               [(eq? (first input) 'let)
                (if (= (length input) 3)
                    (let-exp
                     (map first (second input))
                     (map (λ (bind) (parse (second bind)))
                          (second input))
                     (parse (third input)))
                    (error 'parse "Invalid syntax ~s" input))]
               [else (app-exp
                      (parse (first input))
                      (map parse (rest input)))])]
        [else parse-error]))