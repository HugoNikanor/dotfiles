#!/usr/bin/guile \
-s
!#

(add-to-load-path (dirname (current-filename)))


(use-modules (conf-base)
             (ice-9 popen)
             (ice-9 rdelim)
             ((mutt) #:prefix mutt:)
             ((mbsync) #:prefix mbsync:))



(define $HOME (getenv "HOME"))

(account default ()
         (name "Hugo Hörnquist")
         (path-base ,(case (string->symbol (gethostname))
                       ((gandalf) (path-append "/var/mail" (getlogin)))
                       (else (path-append $HOME "/.local/var/mail"))))

         (pass ,(string-append "pass " (? pass-path)))

         (IMAPAccount
          (SSLType "IMAPS")
          (CertificateFile "/etc/ssl/certs/ca-certificates.crt")
          (User ,(? address))
          (PassCmd ,(format #f "+\"~a\"" (? pass))))

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
          (Master ,(format #f ":~a:" (mbsync:category-transformer 'IMAPStore (? acc-name))))
          (Slave  ,(format #f ":~a:" (mbsync:category-transformer 'MaildirStore (? acc-name)))))

         (signature ,(? name))

         (mutt
          (set
           (from ,(format #f "~a <~a>" (? name) (? address)))
           (realname ,(? name)))

          (file
           (dir ,(path-append $HOME "/.mutt/"))
           (account-dir ,(path-append (? mutt file dir) "accounts"))
           (signature-dir ,(path-append (? mutt file dir) "signatures"))
           (account-config ,(path-append (? mutt file account-dir) (? address)))
           (account-signature ,(path-append (? mutt file signature-dir) (? address))))

          (imap-pass ,(? pass))
          (imap-addr ,(format #f "imaps://~a@~a"
                              (? IMAPAccount User)
                              (? IMAPAccount Host)))
          (account-hook
           (imap-password ,(list (? mutt imap-addr)
                                 (format #f "set imap_pass='`~a`'"
                                         (? pass)))))

          (folder-hook
           (hk1 ,(list (format #f "(~a|~a)"
                               (? mutt imap-addr)
                               (? MaildirStore Path))
                       (format #f "source ~a" (? mutt file account-config))
                       )))

          (my_hdr (Bcc ,(? address)))
          ;; signature moved outside mutt for easier usage of multiple
          ;; mail clients.
          (signature ,(? signature)))
         )

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

         (mutt (set (hostname "lysator.liu.se"))))

(account outlook (default)
         (IMAPAccount
          (Host "outlook.office365.com")
          (AuthMechs LOGIN)))

(account liu (outlook)
         (address "hugho389@student.liu.se")
         (pass-path "liu/mail/hugho389")

         (signature "Hugo Hörnquist (hugho389)")
         (mutt (set (hostname "liu.se"))))

(account liu-work (outlook)
         (address "hugo.hornquist@liu.se")
         (pass-path "liu/hugho26")

         (IMAPAccount (User "hugho26@liu.se"))
         (mutt (set (hostname "liu.se"))))

(account vg-base (google)
         (address ,(format #f "~a@vastgota.nation.liu.se" (? acc-name)))

         (pass-path ,(format #f "vastgota.nation.liu.se/mail/~a" (? acc-name)))

         (mutt (set (hostname "vastgota.nation.liu.se")))

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

         (mutt (set (hostname "liuformulastudent.se")))

         (signature
           ,(format #f "~a, Team leader IT & Driverless~%✆ ~a"
                    (? name) (getenv "PHONE"))))



;; check required environment:
(define required-env '("PHONE"))
(for-each (lambda (str)
            (unless (getenv str)
              (format (current-error-port)
                      "~%Please set the env var ~a to something~%~%"
                      str)
              (exit 1)))
          required-env)

(define domainname (read-line (open-input-pipe "hostname --domain")))

(with-output-to-file (path-append $HOME ".mbsyncrc")
  (lambda ()
    (apply mbsync:render
     ((if (not (string=? domainname "lysator.liu.se"))
          (lambda a (cons lysator a))
          list)
      gmail liu liu-work guckel liu-fs))))

(mutt:render
 (open-input-file (path-append (dirname (dirname (current-filename))) "mutt" "muttrc"))
 lysator gmail liu liu-work guckel liu-fs)
