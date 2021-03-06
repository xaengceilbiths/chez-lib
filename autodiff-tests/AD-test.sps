#!chezscheme
;; Simple tests of AD for automatic differentiation
;; Examples from https://www.sintef.no/globalassets/project/evitameeting/2010/ad2010.pdf

(import (except (scheme base) + - * / expt square = < > <= >= zero? positive? negative? real?)
        (autodiff AD)
        (surfage s64 testing)
        (robin srfi64-utils))

(test-begin "autodiff-AD")

(test-equal 6 ((derivative-F (lambda (x) (* x x))) 3))
(test-equal 13/18 ((derivative-F (lambda (x) (/ (* (+ x 1) (- x 2))
                                                (+ x 3)))) 3))
(test-equal 13/18 ((derivative-F (lambda (x) (- x (/ (+ (* 4 x) 2)
                      (+ x 3))))) 3))
(test-approx-same 2.0 ((derivative-F (lambda (x) (* (+ 1 x (exp x)) (sin x))))
                       0))

(test-end)

