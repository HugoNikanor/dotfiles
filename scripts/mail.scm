#!/usr/bin/guile \
-s
!#

(add-to-load-path (dirname (current-filename)))


(use-modules (conf-base)
             (ice-9 popen)
             (ice-9 rdelim)
             ((mutt) #:prefix mutt:)
             ((mbsync) #:prefix mbsync:))



(account default ()
         (name "Hugo Hörnquist")
         (path-base ,(or (getenv "MAILDIR") (path-append (getenv "HOME") "/.local/var/mail")))

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
           (dir ,(path-append (getenv "HOME") "/.mutt/"))
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

         (signature "hugo Hörnquist"))

(account liu (default)
         (address "hugho389@student.liu.se")
         (pass-path "liu/mail/hugho389")

         (IMAPAccount
          (Host "outlook.office365.com")
          (AuthMechs LOGIN))

         (signature "Hugo Hörnquist (hugho389)"))

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

         (signature "Hugo Hörnquist, IT med mera."))



(with-output-to-file (path-append (getenv "HOME") ".mbsyncrc")
  (lambda ()
    (let ((domainname (read-line (open-input-pipe "hostname --domain"))))
      (cond [(string=? domainname "lysator.liu.se")
             (mbsync:render
               gmail liu guckel liu-fs)]
            [else
              (mbsync:render
                gmail liu lysator guckel liu-fs)]))))

(mutt:render
 (open-input-file (path-append (dirname (dirname (current-filename))) "mutt" "muttrc"))
 lysator gmail liu guckel liu-fs)
