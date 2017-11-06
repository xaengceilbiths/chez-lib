#!chezscheme
;; Trivial implementation of Ephemerons
;; -- needs replacing with implementation-specific version

(define-library
  (scheme ephemeron)
  (export make-ephemeron
          ephemeron?
          ephemeron-broken?
          ephemeron-key
          ephemeron-datum
          reference-barrier)
  (import (scheme base))

  (begin

    (define-record-type <ephemeron>
                        (%make-ephemeron key datum broken?)
                        ephemeron?
                        (key ephemeron-key)
                        (datum ephemeron-datum)
                        (broken? ephemeron-broken?))

    (define (make-ephemeron key datum) (%make-ephemeron key datum #f))

    (define (reference-barrier key) #t)

    ))

