(define-module (mutt)
  #:use-module (conf-base)
  #:use-module (ice-9 rdelim)
  #:use-module (ice-9 format)
  #:export (render))

(define (render-mutt-account account)
  (let ((o (instanciate account)))

    (mkdir-p (get-field o '(mutt file account-dir)))
    (mkdir-p (get-field o '(mutt file signature-dir)))

    (with-output-to-file (get-field o '(mutt file account-config))
      (lambda ()
        (format #t "set signature=~a~%" (get-field o '(mutt file account-signature)))
        (ignore-error
         (map-subtree o '(mutt my_hdr)
                      (lambda (name value)
                        (format #t "my_hdr ~a: ~a~%" name value))))
        (map-subtree o '(mutt set)
                     (lambda (name value)
                       (format #t "set ~a = ~a~%" name
                               (if (and (string? value) (string-contains value " "))
                                   (string-append "\"" value "\"")
                                   value))))))

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


(define (render-mutt-global account-class)
  (define account (instanciate account-class))

  (ignore-error
   (map-subtree account '(my_hdr)
                (lambda (name value)
                  (format #t "my_hdr ~a: ~a~%" name value))))

  (map-subtree account '(set)
               (lambda (name value)
                 (format #t "set ~a = ~a~%" name
                         (cond [(symbol? value) value]
                               [(string-contains value " ")
                                (string-append "\"" value "\"")]
                               [else value]))))
  (map-subtree account '(macro)
               (lambda (name value-list)
                 (for-each (lambda (value)
                             (format #t "macro ~a ~a~%" name value))
                           value-list)))

  (for-each (lambda (value) (format #t "source ~a~%" value))
            (get-field account '(source)))

  (map-subtree account '(other)
               (lambda (key value)
                 (format #t "~a ~a~%" key value))))


(define (render base-acc accounts)
  (define muttrc (path-append (getenv "HOME") "/.mutt/muttrc"))
  (mkdir-p (path-append (getenv "HOME") ".mutt"))
  (catch 'system-error
         (lambda () (chmod muttrc #o600))
         (lambda _ 'ignore))
  (with-output-to-file muttrc
    (lambda ()
      ;; (display (read-string base-port))
      (format #t "# This file is autogenerated~%# DON'T EDIT BY HAND~%# Generated ~a~%~%"
              (strftime "%c" (localtime (current-time))))
      (render-mutt-global base-acc)

      (format #t "~%# ~a~%~%" (make-string 50 #\-))
      (for-each (lambda (acc)
                  (let* ((folder-hooks account-hooks (render-mutt-account acc)))
                    (for-each display folder-hooks)
                    (for-each display account-hooks)))
                accounts)
      (format #t "mailboxes ~{~a~^ \\~%          ~}~%"
              (map (lambda (o) (get-field o '(mutt imap-addr))) (map instanciate accounts)))))
  (chmod muttrc #o400)
  )

