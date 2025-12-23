(use-modules (srfi srfi-1)
             ((ice-9 pretty-print)
              :select (truncated-print))
             ((system repl common)
              :select (repl-default-option-set!
                       repl-welcome)))

(repl-default-option-set!
 'print
 (lambda (repl obj)
   (truncated-print obj)
   (newline)))

;;; Added with Guile 3.0.11
(catch 'misc-error
  (lambda ()
    (read-enable 'bytestrings)
    (print-enable 'bytestrings))
  (lambda _ 'noop))

(set! repl-welcome (const #t))
