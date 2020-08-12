(define-module (files)
  #:use-module (conf-base)
  #:export (ensure-files))

(define (ensure-files . accounts)
  (for-each
    (lambda (acc)
      (let ((o (instanciate acc)))
        ;; TODO map ofer all files. However, map-subtree seems broken
        (with-output-to-file (get-field o '(files color path))
          (lambda () (display (get-field o '(files color content)))
            (newline)))))
    accounts))
