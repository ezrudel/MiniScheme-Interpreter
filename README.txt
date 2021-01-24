MiniScheme is a project I did for a Programming Abstractions class. We used Racket to learn about functional programming. MiniScheme is a version of the Scheme language with pared-down functionality. MiniScheme uses syntax similar to Racket, and supports numerical/primitive operations, variables, lambda/application expressions, if-then-else, let, letrec, set!, and begin.

The repository contains the following files:
MiniScheme.rkt
parse.rkt
interp.rkt
env.rkt
tests.rkt
parse-tests.rkt
interp-tests.rkt
env-tests.rkt

MiniScheme.rkt is the user interface file. Here is some example code to test it:
MS> (letrec ([fac (lambda (x) (if (eqv? x 0) 1 (* x (fac (sub1 x)))))]) (fac 4))
        24
MS> (letrec ([fac (lambda (x) (if (eqv? x 0) 1 (* x (fac (sub1 x)))))]) (fac 10))
        3628800
MS> (letrec ([even? (lambda (n) (if (eqv? 0 n) True (odd? (sub1 n))))]  
             [odd? (lambda (n) (if (eqv? 0 n) False (even? (sub1 n))))] )
   (even? 5))
False

The assignment directions can be found here:
https://checkoway.net/teaching/cs275/2020-fall/minischeme.html