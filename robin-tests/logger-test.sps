#!chezscheme
(import (scheme base)
        (robin logger)
        (surfage s64 testing))

(test-begin "robin-logger")

(define *output-port* (open-output-string))
(define *log* (new-logger *output-port*))
(log-level *log* 'info)
(test-assert (log-info? *log*))
(test-assert (log-warn? *log*))
(test-assert (not (log-debug? *log*)))
(log-warn *log* "abc")
(log-fatal *log* "very bad")
(log-debug *log* "ignored")
(log-level *log* 'debug)
(test-assert (log-debug? *log*))
(log-debug *log* "shown")
(log-close *log*)
(test-equal (get-output-string *output-port*)
            "warn: abc\nfatal: very bad\ndebug: shown\n")

(test-end)
