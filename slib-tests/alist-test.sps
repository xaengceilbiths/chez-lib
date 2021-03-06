#!chezscheme
(import (scheme base)
        (scheme char)
        (slib alist)
        (surfage s64 testing))

(test-begin "slib-alist")

(define put (alist-associator string-ci=?))
(define alist '())
(set! alist (put alist "Adam" 9))
(set! alist (put alist "Ben" 19))
(set! alist (put alist "Charles" 19))
(test-equal 9 ((alist-inquirer string-ci=?) alist "adam"))
(test-equal #f ((alist-inquirer string=?) alist "adam"))
(test-equal (cons "Ben" 19) ((predicate->asso string-ci=?) "ben" alist))
(set! alist ((alist-remover string-ci=?) alist "ben"))
(test-equal #f ((predicate->asso string-ci=?) "ben" alist))
(test-equal 9 ((alist-inquirer string-ci=?) alist "adam"))
(test-equal 19 ((alist-inquirer string-ci=?) alist "charles"))
(define alist2 (alist-map (lambda (key val) (+ 1 val)) alist))
(test-equal 9 ((alist-inquirer string-ci=?) alist "adam"))
(test-equal 10 ((alist-inquirer string-ci=?) alist2 "adam"))
(let ((total 0))
  (alist-for-each (lambda (key val) (set! total (+ total val)))
                  alist)
  (test-equal 28 total))

(test-end)
