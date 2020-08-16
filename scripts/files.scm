(define-module (files)
  #:use-module (conf-base)
  #:export (ensure-files))

(define (ensure-files . accounts)
  (for-each
    (lambda (acc)
      (let ((o (instanciate acc)))
        ;; TODO map over all files. However, map-subtree seems broken
        (mkdir-p (dirname (get-field o '(files color path))))
        (with-output-to-file (get-field o '(files color path))
          (lambda () (display (get-field o '(files color content)))
            (newline)))))
    accounts))
