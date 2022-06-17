(define-module (files)
  #:use-module (conf-base)
  #:export (ensure-files))

(define (ensure-files destdir . accounts)
  (for-each
    (lambda (acc)
      (let ((o (instanciate acc)))
        ;; TODO map over all files. However, map-subtree seems broken
        (catch 'misc-error
               (lambda ()
                 (define f (path-append
                             destdir (get-field o '(files color path))))
                 (define content (get-field o '(files color content)))
                 (mkdir-p (dirname f))
                 (with-output-to-file f
                   (lambda () (display content) (newline))))
               (lambda (err proc fmt args data)
                 (format #t "~a in ~a (when processing ~s):~%~?~%" err proc (procedure-name acc) fmt args)))))
    accounts))
