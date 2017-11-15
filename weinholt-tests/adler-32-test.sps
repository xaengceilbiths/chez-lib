#!chezscheme

(import (scheme base)
        (weinholt adler-32)
        (surfage s64 testing))

(test-begin "weinholt-adler-32")

(test-equal (adler-32 (string->utf8 "123456789")) #x91E01DE)

(test-end)
