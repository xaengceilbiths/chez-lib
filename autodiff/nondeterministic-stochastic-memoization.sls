#!chezscheme
;Thursday 20 May 2010
;
;Jeffrey Mark Siskind
;School of Electrical and Computer Engineering
;Purdue University
;465 Northwestern Avenue
;West Lafayette IN 47907-2035 USA
;voice: 765/496-3197
;fax: 765/494-6440
;qobi@purdue.edu
;http://engineering.purdue.edu/~qobi
;
;The entire contents of this directory copyright 2010 Purdue University. All
;rights reserved.
;
;These are some AD (Automatic Differentiation) tools for both forward
;and reverse mode written for R6RS Scheme.  They run under Ikarus and
;PLT Scheme.  Also included are some experimental packages to support
;nondeterministic and stochastic programming, including constraint
;satisfaction problems.
;
;This code is completely unsupported.
;
;It is licensed under the GPL v2 or later, see the enclosed file COPYING.
;
;This material is based upon work supported by the National Science
;Foundation under Grant No. 0438806.
;Any opinions, findings, and conclusions or recommendations expressed in
;this material are those of the author(s) and do not necessarily reflect
;the views of the National Science Foundation.

;; Packaged for R7RS Scheme by Peter Lane, 2017

(library
 (autodiff nondeterministic-stochastic-memoization)
 (export memoize-nondeterministic-stochastic
	 memoize-nondeterministic-removing-duplicates-stochastic
	 memoize-nondeterministic-stochastic-coalescing-duplicates
	 memoize-nondeterministic-removing-duplicates-stochastic-coalescing-duplicates
	 memoize-stochastic-nondeterministic
	 memoize-stochastic-coalescing-duplicates-nondeterministic
	 memoize-stochastic-nondeterministic-removing-duplicates
	 memoize-stochastic-coalescing-duplicates-nondeterministic-removing-duplicates)
 (import (scheme base)
	 (autodiff nondeterministic-scheme)
	 (autodiff stochastic-scheme)
	 (autodiff deterministic-memoization))

 (begin

   (define (memoize-nondeterministic-stochastic f)
     (let ((f (memoize
                (lambda arguments (distribution (domain (apply f arguments)))))))
       (lambda arguments (a-member-of (draw (apply f arguments))))))

   (define (memoize-nondeterministic-removing-duplicates-stochastic f)
     (let ((f (memoize
                (lambda arguments
                  (distribution (remove-duplicates (domain (apply f arguments))))))))
       (lambda arguments (a-member-of (draw (apply f arguments))))))

   (define (memoize-nondeterministic-stochastic-coalescing-duplicates f)
     (let ((f (memoize
                (lambda arguments
                  (coalesce-duplicates
                    (distribution (domain (apply f arguments))))))))
       (lambda arguments (a-member-of (draw (apply f arguments))))))

   (define (memoize-nondeterministic-removing-duplicates-stochastic-coalescing-duplicates f)
     (let ((f (memoize
                (lambda arguments
                  (coalesce-duplicates
                    (distribution
                      (remove-duplicates (domain (apply f arguments)))))))))
       (lambda arguments (a-member-of (draw (apply f arguments))))))

   (define (memoize-stochastic-nondeterministic f)
     (let ((f (memoize
                (lambda arguments (domain (distribution (apply f arguments)))))))
       (lambda arguments (draw (a-member-of (apply f arguments))))))

   (define (memoize-stochastic-coalescing-duplicates-nondeterministic f)
     (let ((f (memoize
                (lambda arguments
                  (domain
                    (coalesce-duplicates (distribution (apply f arguments))))))))
       (lambda arguments (draw (a-member-of (apply f arguments))))))

   (define (memoize-stochastic-nondeterministic-removing-duplicates f)
     (let ((f (memoize
                (lambda arguments
                  (remove-duplicates (domain (distribution (apply f arguments))))))))
       (lambda arguments (draw (a-member-of (apply f arguments))))))

   (define (memoize-stochastic-coalescing-duplicates-nondeterministic-removing-duplicates f)
     (let ((f (memoize
                (lambda arguments
                  (remove-duplicates
                    (domain
                      (coalesce-duplicates (distribution (apply f arguments)))))))))
       (lambda arguments (draw (a-member-of (apply f arguments))))))

   ))

