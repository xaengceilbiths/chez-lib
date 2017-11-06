#!chezscheme

(library (scheme lazy)
         (export delay eager force lazy)
         (import (r7b-impl lazy)))
