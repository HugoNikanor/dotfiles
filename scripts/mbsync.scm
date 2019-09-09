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
  (let ((alist (instanciate account)))
    (for-each
     (lambda (category)
       (format #t "~%~a ~a~%" category (category-transformer
                                        category
                                        (get-field alist '(toplevel acc-name))))
       (for-each
        (match-lambda ((key proc)
                       (format #t "~a ~a~%"
                               key (proc alist))))
        (assoc-ref alist category)))
     '(IMAPAccount MaildirStore IMAPStore Channel))
    (format #t "# ~a~%" (make-string 40 #\-))))


(define (render-mbsync-accounts . accounts)
  (for-each render-mbsync-account accounts)
  (format #t "~%Group all~%")
  (for-each (lambda (name) (format #t "Channel ~a~%" name))
            (map procedure-name accounts)))



(account default ()
         (toplevel
          (name "Hugo Hörnquist")
          (path-base "~/mail/"))

         (IMAPAccount
          (SSLType "IMAPS")
          (CertificateFile "/etc/ssl/certs/ca-certificates.crt")
          (User ,(? address))
          (PassCmd ,(format #f "+\"pass ~a\"" (? pass-path))))

         (MaildirStore
          (AltMap yes)
          (Path ,(path-append (? path-base) (string-titlecase (? acc-name)) "/"))
          (Inbox ,(path-append (? (MaildirStore Path))
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
          ;; set
          (from ,(format #f "~a <~a>" (? name) (? address)))
          (folder "~/mail")
          (record ,(path-append (? (mutt folder)) "/sent"))
          (postponed ,(path-append (? (mutt folder)) "/postponed"))
          (realname (? name))

          (my_hdr ,(format #f "Bcc: <~a>" (? address)))
          (signature ,(? name))
          ))

(account google (default)
         (IMAPAccount
          (Host "imap.gmail.com")
          (AuthMechs "LOGIN"))
         (Channel
          (Patterns "* ![Gmail]*")))




(account gmail (google)
         (toplevel
          (address "hugo.hornquist@gmail.com")
          (pass-path "google.com/hugo.hornquist"))

         (mutt
          (from "Hugo Hörnquist <hugo@hornquist.se>")))

(account lysator (default)
         (toplevel
          (address "hugo@lysator.liu.se")
          (pass-path "lysator/mail/hugo"))

         (IMAPAccount
          (Host "imap.lysator.liu.se"))

         (mutt
          (signature "hugo Hörnquist")))

(account liu (default)
         (toplevel
          (address "hugho389@student.liu.se")
          (pass-path "liu/mail/hugho389"))

         (IMAPAccount
          (Host "outlook.office365.com")
          (AuthMechs LOGIN))

         (mutt
          (signature "Hugo Hörnquist (hugho389)")))

(account vg-base (google)
         (toplevel
          (pass-path ,(format #f "vastgota.nation.liu.se/mail/~a" (? acc-name))))

         (MaildirStore
           (Path ,(format #f "~aVastgota.~a/"
                          (? path-base)
                          (string-titlecase (? acc-name)))))

         (IMAPAccount
          (User ,(format #f "~a@vastgota.nation.liu.se" (? acc-name)))))

(account guckel (vg-base))

(account liu-fs (google)
         (toplevel
           (pass-path "formulastudent/google/hugo.hornquist")
           (address "hugo.hornquist@liuformulastudent.se"))

         (MaildirStore
           (Path ,(format #f "~aFormulastudent/"
                          (? path-base))))


         (mutt
           (signature "Hugo Hörnquist, IT med mera.)")))




#;
(define (render-mutt-configs . accounts)
  (let ((o (instanciate account)))
    (get-field )
    ))


(render-mbsync-accounts
 lysator
 gmail
 liu
 guckel
 liu-fs)

#;
(render-mutt-configs
 lysator
 gmail
 liu
 guckel
 )

