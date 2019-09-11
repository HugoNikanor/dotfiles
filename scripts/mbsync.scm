#!/usr/bin/guile \
-s
!#

(add-to-load-path (dirname (current-filename)))

(use-modules (conf-base)
             (ice-9 match))



(define (category-transformer category name)
  (string-append
   name
   (case category
     [(MaildirStore) "-local"]
     [(IMAPStore)   "-remote"]
     [else ""]
     )))

(define (render-mbsync-account account)
  (let ((obj (instanciate account)))
    (for-each
     (lambda (category)
       (format #t "~%~a ~a~%" category (category-transformer
                                        category
                                        (get-field obj '(acc-name))))
       (for-each
        (match-lambda ((key proc)
                       (format #t "~a ~a~%"
                               key (proc obj))))
        (car (assoc-ref obj category))
        #; (get-field alist (list category))
        ))
     '(IMAPAccount MaildirStore IMAPStore Channel))
    (format #t "# ~a~%" (make-string 40 #\-))
    obj))


(define (render-mbsync-accounts . accounts)
  (let ((objects (map render-mbsync-account accounts)))
    (format #t "~%Group all~%")
    (for-each (lambda (name) (format #t "Channel ~a~%" name))
              (map (lambda (o) (get-field o '(acc-name))) objects))))



(account default ()
         (name "Hugo Hörnquist")
         (path-base ,(or (getenv "MAILDIR") "~/mail/"))

         (IMAPAccount
          (SSLType "IMAPS")
          (CertificateFile "/etc/ssl/certs/ca-certificates.crt")
          (User ,(? address))
          (PassCmd ,(format #f "+\"pass ~a\"" (? pass-path))))

         (MaildirStore
          (AltMap yes)
          (Path ,(path-append (? path-base) (string-titlecase (? acc-name)) "/"))
          (Inbox ,(path-append (? MaildirStore Path)
                                 "INBOX"))
          (SubFolders Verbatim))

         (IMAPStore
          (Account ,(? acc-name)))

         (Channel
          (Create Both)
          (Sync All)
          (Patterns "*")
          (SyncState "*")
          (Master ,(format #f ":~a:" (category-transformer 'IMAPStore (? acc-name))))
          (Slave  ,(format #f ":~a:" (category-transformer 'MaildirStore (? acc-name)))))

         (mutt
          (set
           (from ,(format #f "~a <~a>" (? name) (? address)))
           (realname ,(? name)))

          (my_hdr (Bcc ,(? address)))
          (signature ,(? name))
          ))

(account google (default)
         (IMAPAccount
          (Host "imap.gmail.com")
          (AuthMechs "LOGIN"))
         (Channel
          (Patterns "* ![Gmail]*")))




(account gmail (google)
         (address "hugo.hornquist@gmail.com")
         (pass-path "google.com/hugo.hornquist")

         (mutt
          (set
           (from "Hugo Hörnquist <hugo@hornquist.se>"))))

(account lysator (default)
         (address "hugo@lysator.liu.se")
         (pass-path "lysator/mail/hugo")

         (IMAPAccount
          (Host "imap.lysator.liu.se"))

         (mutt
          (set
           (signature "hugo Hörnquist"))))

(account liu (default)
         (address "hugho389@student.liu.se")
         (pass-path "liu/mail/hugho389")

         (IMAPAccount
          (Host "outlook.office365.com")
          (AuthMechs LOGIN))

         (mutt
          (set
           (signature "Hugo Hörnquist (hugho389)"))))

(account vg-base (google)
         (address ,(format #f "~a@vastgota.nation.liu.se" (? acc-name)))

         (pass-path ,(format #f "vastgota.nation.liu.se/mail/~a" (? acc-name)))

         (MaildirStore
           (Path ,(path-append (? path-base)
                               (string-append "Vastgota."
                                 (string-titlecase (? acc-name)))
                               "/"))))

(account guckel (vg-base))

(account liu-fs (google)
         (pass-path "formulastudent/google/hugo.hornquist")
         (address "hugo.hornquist@liuformulastudent.se")

         (MaildirStore
           (Path ,(path-append (? path-base) "Formulastudent/")))


         (mutt
          (set
           (signature "Hugo Hörnquist, IT med mera."))))


(account mutt-base ()
         (mutt (set
                (folder "~/mail")
                (record ,(path-append (? mutt set folder) "/sent"))
                (postponed ,(path-append (? mutt set folder) "/postponed")))))


(define (render-mutt-set account)
  (let ((o (instanciate account)))
    (let ((subtree (get-field o '(mutt set))))
      (for-each (lambda (name)
                  (format #t "set ~a = \"~a\"~%" name (get-field o (list 'mutt 'set name))))
                (map car subtree)))))

(define (render-mutt-configs base . accounts)
  (render-mutt-set base)
  (format #t "~a~%" (make-string 40 #\-))
  (for-each render-mutt-set accounts))



#;
(render-mbsync-accounts
 lysator
 gmail
 liu
 guckel
 liu-fs)



(render-mutt-configs
 lysator
 gmail
 liu
 guckel
 )

