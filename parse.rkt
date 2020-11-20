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

; lambda-exp data type
(define (lambda-exp params body)
  (list 'lam-exp params body))

(define (lambda-exp? exp)
  (if (list? exp)
      (if (equal? 'lam-exp (first exp))
          #t
          #f)
      #f))

(define (lambda-params exp)
  (if (lambda-exp? exp)
      (second exp)
      (error "~s is not a lambda-exp" exp)))

(define (lambda-body exp)
  (if (lambda-exp? exp)
      (third exp)
      (error "~s is not a lambda-exp" exp)))

; set-exp data type
(define (set-exp sym exp)
  (list 'set-exp sym exp))

(define (set-exp? exp)
  (if (list? exp)
      (if (equal? 'set-exp (first exp))
          #t
          #f)
      #f))

(define (set-exp-sym exp)
  (if (set-exp? exp)
      (second exp)
      (error "~s is not a set-exp" exp)))

(define (set-exp-val exp)
  (if (set-exp? exp)
      (third exp)
      (error "~s is not a set-exp" exp)))

; begin-exp data type
(define (begin-exp exps)
  (list 'begin-exp exps))

(define (begin-exp? exp)
  (if (list? exp)
      (if (equal? 'begin-exp (first exp))
          #t
          #f)
      #f))

(define (begin-exp-list exp)
  (if (begin-exp exp)
      (second exp)
      (error "~s is not a begin-exp" exp)))


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
               [(eq? (first input) 'lambda)
                (if (= (length input) 3)
                    (lambda-exp (second input)
                                (parse (third input)))
                    (error 'parse "Invalid syntax ~s" input))]
               [(eq? (first input) 'set!)
                (if (= (length input) 3)
                    (set-exp (second input)
                             (parse (third input)))
                    (error 'parse "Invalid syntax ~s" input))]
               [(eq? (first input) 'begin)
                (begin-exp (map parse (rest input)))]
               [else (app-exp
                      (parse (first input))
                      (map parse (rest input)))])]
        [else parse-error]))
