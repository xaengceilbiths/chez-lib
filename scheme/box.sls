#!chezscheme
;; Reference implementation for srfi 111

(library 
  (scheme box)
  (export box box? unbox set-box!)
  (import (scheme base))

  (begin
    (define-record-type box-type
                        (box value)
                        box?
                        (value unbox set-box!))))
