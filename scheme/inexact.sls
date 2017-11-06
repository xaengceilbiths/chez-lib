#!chezscheme

(library (scheme inexact)
         (export
;; from R7RS draft 4
acos asin atan cos exp finite?  log nan? sin sqrt tan
infinite?
 )
         (import (r7b-impl inexact)
		 (only (chezscheme) infinite?)))
