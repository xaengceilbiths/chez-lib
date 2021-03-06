#!chezscheme
;; Copyright © 2010 Göran Weinholt <goran@weinholt.se>

;; Permission is hereby granted, free of charge, to any person obtaining a
;; copy of this software and associated documentation files (the "Software"),
;; to deal in the Software without restriction, including without limitation
;; the rights to use, copy, modify, merge, publish, distribute, sublicense,
;; and/or sell copies of the Software, and to permit persons to whom the
;; Software is furnished to do so, subject to the following conditions:

;; The above copyright notice and this permission notice shall be included in
;; all copies or substantial portions of the Software.

;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL
;; THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
;; FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
;; DEALINGS IN THE SOFTWARE.

;; Packaged for R7RS Scheme and SRFI 64 by Peter Lane, 2017

(import (weinholt arcfour)
        (surfage s64 testing)
        (scheme base))

(define (arcfour pt key)
  (let ((ct (make-bytevector (bytevector-length pt) 0))
        (ks (expand-arcfour-key key)))
    (arcfour! pt 0 ct 0 (bytevector-length pt) ks)
    (clear-arcfour-keystream! ks)
    ct))

(test-begin "weinholt-arcfour")

;; Test vectors from draft-kaukonen-cipher-arcfour-03.txt

(test-equal (arcfour #u8(#x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00)
                     #u8(#x01 #x23 #x45 #x67 #x89 #xAB #xCD #xEF))
            #u8(#x74 #x94 #xC2 #xE7 #x10 #x4B #x08 #x79))

(test-equal (arcfour #u8(#xdc #xee #x4c #xf9 #x2c)
                     #u8(#x61 #x8a #x63 #xd2 #xfb))
            #u8(#xf1 #x38 #x29 #xc9 #xde))

(test-equal (arcfour #u8(#x52 #x75 #x69 #x73 #x6c #x69 #x6e #x6e
                         #x75 #x6e #x20 #x6c #x61 #x75 #x6c #x75
                         #x20 #x6b #x6f #x72 #x76 #x69 #x73 #x73
                         #x73 #x61 #x6e #x69 #x2c #x20 #x74 #xe4
                         #x68 #x6b #xe4 #x70 #xe4 #x69 #x64 #x65
                         #x6e #x20 #x70 #xe4 #xe4 #x6c #x6c #xe4
                         #x20 #x74 #xe4 #x79 #x73 #x69 #x6b #x75
                         #x75 #x2e #x20 #x4b #x65 #x73 #xe4 #x79
                         #xf6 #x6e #x20 #x6f #x6e #x20 #x6f #x6e
                         #x6e #x69 #x20 #x6f #x6d #x61 #x6e #x61
                         #x6e #x69 #x2c #x20 #x6b #x61 #x73 #x6b
                         #x69 #x73 #x61 #x76 #x75 #x75 #x6e #x20
                         #x6c #x61 #x61 #x6b #x73 #x6f #x74 #x20
                         #x76 #x65 #x72 #x68 #x6f #x75 #x75 #x2e
                         #x20 #x45 #x6e #x20 #x6d #x61 #x20 #x69
                         #x6c #x6f #x69 #x74 #x73 #x65 #x2c #x20
                         #x73 #x75 #x72 #x65 #x20 #x68 #x75 #x6f
                         #x6b #x61 #x61 #x2c #x20 #x6d #x75 #x74
                         #x74 #x61 #x20 #x6d #x65 #x74 #x73 #xe4
                         #x6e #x20 #x74 #x75 #x6d #x6d #x75 #x75
                         #x73 #x20 #x6d #x75 #x6c #x6c #x65 #x20
                         #x74 #x75 #x6f #x6b #x61 #x61 #x2e #x20
                         #x50 #x75 #x75 #x6e #x74 #x6f #x20 #x70
                         #x69 #x6c #x76 #x65 #x6e #x2c #x20 #x6d
                         #x69 #x20 #x68 #x75 #x6b #x6b #x75 #x75
                         #x2c #x20 #x73 #x69 #x69 #x6e #x74 #x6f
                         #x20 #x76 #x61 #x72 #x61 #x6e #x20 #x74
                         #x75 #x75 #x6c #x69 #x73 #x65 #x6e #x2c
                         #x20 #x6d #x69 #x20 #x6e #x75 #x6b #x6b
                         #x75 #x75 #x2e #x20 #x54 #x75 #x6f #x6b
                         #x73 #x75 #x74 #x20 #x76 #x61 #x6e #x61
                         #x6d #x6f #x6e #x20 #x6a #x61 #x20 #x76
                         #x61 #x72 #x6a #x6f #x74 #x20 #x76 #x65
                         #x65 #x6e #x2c #x20 #x6e #x69 #x69 #x73
                         #x74 #xe4 #x20 #x73 #x79 #x64 #xe4 #x6d
                         #x65 #x6e #x69 #x20 #x6c #x61 #x75 #x6c
                         #x75 #x6e #x20 #x74 #x65 #x65 #x6e #x2e
                         #x20 #x2d #x20 #x45 #x69 #x6e #x6f #x20
                         #x4c #x65 #x69 #x6e #x6f)
                     #u8(#x29 #x04 #x19 #x72 #xfb #x42 #xba #x5f
                         #xc7 #x12 #x77 #x12 #xf1 #x38 #x29 #xc9))
            #u8(#x35 #x81 #x86 #x99 #x90 #x01 #xe6 #xb5
                #xda #xf0 #x5e #xce #xeb #x7e #xee #x21
                #xe0 #x68 #x9c #x1f #x00 #xee #xa8 #x1f
                #x7d #xd2 #xca #xae #xe1 #xd2 #x76 #x3e
                #x68 #xaf #x0e #xad #x33 #xd6 #x6c #x26
                #x8b #xc9 #x46 #xc4 #x84 #xfb #xe9 #x4c
                #x5f #x5e #x0b #x86 #xa5 #x92 #x79 #xe4
                #xf8 #x24 #xe7 #xa6 #x40 #xbd #x22 #x32
                #x10 #xb0 #xa6 #x11 #x60 #xb7 #xbc #xe9
                #x86 #xea #x65 #x68 #x80 #x03 #x59 #x6b
                #x63 #x0a #x6b #x90 #xf8 #xe0 #xca #xf6
                #x91 #x2a #x98 #xeb #x87 #x21 #x76 #xe8
                #x3c #x20 #x2c #xaa #x64 #x16 #x6d #x2c
                #xce #x57 #xff #x1b #xca #x57 #xb2 #x13
                #xf0 #xed #x1a #xa7 #x2f #xb8 #xea #x52
                #xb0 #xbe #x01 #xcd #x1e #x41 #x28 #x67
                #x72 #x0b #x32 #x6e #xb3 #x89 #xd0 #x11
                #xbd #x70 #xd8 #xaf #x03 #x5f #xb0 #xd8
                #x58 #x9d #xbc #xe3 #xc6 #x66 #xf5 #xea
                #x8d #x4c #x79 #x54 #xc5 #x0c #x3f #x34
                #x0b #x04 #x67 #xf8 #x1b #x42 #x59 #x61
                #xc1 #x18 #x43 #x07 #x4d #xf6 #x20 #xf2
                #x08 #x40 #x4b #x39 #x4c #xf9 #xd3 #x7f
                #xf5 #x4b #x5f #x1a #xd8 #xf6 #xea #x7d
                #xa3 #xc5 #x61 #xdf #xa7 #x28 #x1f #x96
                #x44 #x63 #xd2 #xcc #x35 #xa4 #xd1 #xb0
                #x34 #x90 #xde #xc5 #x1b #x07 #x11 #xfb
                #xd6 #xf5 #x5f #x79 #x23 #x4d #x5b #x7c
                #x76 #x66 #x22 #xa6 #x6d #xe9 #x2b #xe9
                #x96 #x46 #x1d #x5e #x4d #xc8 #x78 #xef
                #x9b #xca #x03 #x05 #x21 #xe8 #x35 #x1e
                #x4b #xae #xd2 #xfd #x04 #xf9 #x46 #x73
                #x68 #xc4 #xad #x6a #xc1 #x86 #xd0 #x82
                #x45 #xb2 #x63 #xa2 #x66 #x6d #x1f #x6c
                #x54 #x20 #xf1 #x59 #x9d #xfd #x9f #x43
                #x89 #x21 #xc2 #xf5 #xa4 #x63 #x93 #x8c
                #xe0 #x98 #x22 #x65 #xee #xf7 #x01 #x79
                #xbc #x55 #x3f #x33 #x9e #xb1 #xa4 #xc1
                #xaf #x5f #x6a #x54 #x7f))

(test-end)

