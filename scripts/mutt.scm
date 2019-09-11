(define-module (mutt)
  #:use-module (conf-base)
  #:export (render))

(define (render-mutt-account account)
  (let ((o (instanciate account)))

    (mkdir-p (get-field o '(mutt file account-dir)))
    (mkdir-p (get-field o '(mutt file signature-dir)))

    (with-output-to-file (get-field o '(mutt file account-config))
      (lambda ()
        (format #t "set signature=~a~%" (get-field o '(mutt file account-signature)))
        (map-subtree o '(mutt my_hdr)
                     (lambda (name value)
                       (format #t "my_hdr ~a: ~a" name value)))
        (map-subtree o '(mutt set)
                     (lambda (name value)
                       (format #f "set ~a = ~a~%" name value)))))

    (with-output-to-file (get-field o '(mutt file account-signature))
      (lambda () (display (get-field o '(mutt signature))) (newline)))

    ;; to main file
    (values
     (map-subtree o '(mutt folder-hook)
                  (lambda (_ fhook)
                    (apply format #f "folder-hook '~a' \"~a\"~%"
                           fhook)))
     (map-subtree o '(mutt account-hook)
                  (lambda (_ ahook)
                    (apply format #f "account-hook \"~a\" \"~a\"~%"
                           ahook))))))


(define (render . accounts)
  (with-output-to-file (path-append (getenv "HOME") "/.mutt/muttrc")
    (lambda ()
      (for-each (lambda (acc)
                  (let* ((folder-hooks account-hooks (render-mutt-account acc)))
                    (for-each display folder-hooks)
                    (for-each display account-hooks)))
                accounts)
      (format #t "mailboxes ~{~a~^ \\~%          ~}~%"
              (map (lambda (o) (get-field o '(mutt imap-addr))) (map instanciate accounts))))))

