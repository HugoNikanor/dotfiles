(define-module (files)
  #:use-module (conf-base)
  #:export (ensure-files))

(define (ensure-files destdir . accounts)
  (for-each
    (lambda (acc)
      (let ((o (instanciate acc)))
        ;; TODO map over all files. However, map-subtree seems broken
        (define f (path-append 
                    destdir (get-field o '(files color path))))
        (mkdir-p (dirname f))
        (with-output-to-file f
          (lambda () (display (get-field o '(files color content)))
            (newline)))))
    accounts))
