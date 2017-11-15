#!chezscheme

(import (scheme base)
        (srfi 31)
        (surfage s64 testing))

(test-begin "srfi-31-rec")

;; make a simple factorial function
(define fact 
  (rec (F N) ; defines and returns fn F
       ((rec (G K L)  ; defines and applies accumulator fn G
             (if (zero? K) L
               (G (- K 1) (* K L))))
        N 1))) ; applies G to initial arguments

(test-assert (procedure? fact))
(test-equal 1 (fact 0))
(test-equal 3628800 (fact 10))

(test-end)

