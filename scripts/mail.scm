#!/usr/bin/guile \
--no-auto-compile -s
!#

(add-to-load-path (dirname (current-filename)))


(use-modules (conf-base)
             (ice-9 popen)
             (ice-9 rdelim)
             ((mutt) #:prefix mutt:)
             ((mbsync) #:prefix mbsync:))



(define $HOME (getenv "HOME"))

(define BINDIR (string-append (dirname (current-filename))
                              file-name-separator-string "bin"))

(define mailfolder
  (case (string->symbol (gethostname))
    ((gandalf) (path-append "/var/mail" (getlogin)))
    (else (path-append $HOME "/.local/var/mail"))))

(account default ()
         (name "Hugo Hörnquist")
         (path-base ,mailfolder)
         (fancy-acc-name ,(string-titlecase (? acc-name)))

         (pass ,(string-append "pass " (? pass-path)))

         (IMAPAccount
          (SSLType "IMAPS")
          (CertificateFile "/etc/ssl/certs/ca-certificates.crt")
          (User ,(? address))
          (PassCmd ,(format #f "+\"~a\"" (? pass))))

         (MaildirStore
          (AltMap yes)
          (Path ,(path-append (? path-base) (? fancy-acc-name) "/"))
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
           (realname ,(? name))
           (record ,(format #f "=~a/INBOX" (? fancy-acc-name))))

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

         (signature "hugo")

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
         (pass ,(format #f "~a/oauth-response liu-imap" BINDIR))
         ;; (pass-path "liu/hugho26")

         (IMAPAccount (User "hugho26@liu.se")
                      ;; NOTE that xouath2 isn't always available.
                      ;; Install something like the aur package
                      ;; cyrus-sasl-xoauth2-git
                      (AuthMechs XOAUTH2))
         (mutt (set (hostname "liu.se"))))

(account vg-base (google)
         (address ,(format #f "~a@vastgota.nation.liu.se" (? acc-name)))
         (postnamn ,(string-titlecase (format #f "~agöte" (? acc-name))))
         (signature ,(format #f "~a, ~a" (? name) (? postnamn)))

         (pass-path ,(format #f "vastgota.nation.liu.se/mail/~a" (? acc-name)))

         (mutt (set (hostname "vastgota.nation.liu.se")
                    (record ,(format #f "=Vastgota.~a/INBOX" (?  fancy-acc-name)))
                    ))

         (MaildirStore
           (Path ,(path-append (? path-base)
                               (string-append "Vastgota."
                                 (? fancy-acc-name))
                               "/"))))

(account guckel (vg-base))
(account valberedningen (vg-base)
         (postnamn "Valberedning"))
(account propaganda (vg-base)
         (signature ,(format #f "~a, ~a?" 
                             (? name)
                             (? postnamn))))
(account qurator (vg-base)
         (postname "Qurator"))

(account formulastudent (google)
         (pass-path "formulastudent/google/hugo.hornquist")
         (address "hugo.hornquist@liuformulastudent.se")

         (mutt (set (hostname "liuformulastudent.se")))

         (signature
           ,(format #f "~a, Team leader IT & Driverless~%✆ ~a"
                    (? name) (getenv "PHONE"))))

(account admittansen (google)
         (pass-path "admittansen/hugo@admittansen.se")
         (address "hugo@admittansen.se")

         (signature "Hugo Hörnquist, Labchef")
         (mutt (set (hostname "admittansen.se"))))



(account mutt-global ()
         (set (sort threads)
              (sort_aux "last-date-received")
              (index_format "%[%y-%m-%d  %H:%M]  %-15.15L %Z %s")
              (status_format "%f (%s) (%?V?limited to '%V'&no limit pattern?) (%P)")
              (menu_scroll yes)
              (editor "vim")
              (send_charset "utf-8")
              (sig_on_top yes)
              (header_cache "~/.mutt/cache")
              (message_cachedir "~/.mutt/cache")
              (folder ,mailfolder)
              (record ,(path-append (? set folder) "sent"))
              (postponed ,(path-append (? set folder) "postponed"))
              (imap_check_subscribed yes)
              (imap_list_subscribed yes)
              (ssl_starttls yes)
              (smtp_url "smtp://hugo@mail.lysator.liu.se:26")
              (smtp_pass "`pass lysator/mail/hugo`")
              (ssl_force_tls yes)
              (assumed_charset "utf-8:iso-8859-1")
              (mbox_type "Maildir")

              (realname "Hugo Hörnquist")
              (from "Hugo Hörnquist <hugo@hornquist.se>")

              (markers no)

              )

         (macro
           (index ,'("\\cb |urlview\n"
                     "\\Ck <save-message>=Lysator/Junk<return>"))
           (pager ,'("\\cb |urlview\n")))

         (source ,'("~/.mutt/vim" "~/.mutt/colors"))

         (other
          (auto_view ,(string-join '("text/html" "text/calendar" "application/ics")))
          (alternative_order ,(string-join '("text/plain" "text/enriched" "text/html")))
          (push "<last-entry>")
          (ignore "*")
          (unignore ,(string-join '("from:" "subject" "to" "cc" "date" "x-url" "user-agent" "x-spam-score:"
                                    "X-Original-To" "Delivered-To"))))
         )

(account mutt-global-lysator (mutt-global)
         (set
           (from "Hugo Hörnquist <hugo@lysator.liu.se>")
           (hostname "lysator.liu.se")

           (mask "!(dovecot|cur|tmp|new)")
           (spoolfile "/mp/mail/hugo/Maildir")
           (mbox      "/mp/mail/hugo/Maildir")
           (folder "~/.local/var/mail/")

           ;; TODO
           ;; (record ...)

           ))

(account mutt-global-gpg (mutt-global)
         (set
           (spoolfile "/var/mail/hugo/Gmail/")
           (crypt_use_gpgme yes)
           (pgp_default_key "E376B3821453F4BE1ED6F3C1265514B158C4CA23")))




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

(define account-list
  (list
    lysator
    gmail
    liu
    liu-work
    guckel
    valberedningen
    propaganda
    formulastudent
    admittansen
    qurator
    ))


(with-output-to-file (path-append $HOME ".mbsyncrc")
  (lambda ()
    (mbsync:render
      (if (string=? domainname "lysator.liu.se")
        (delete lysator account-list)
        account-list))))


(mutt:render
 ;; (open-input-file (path-append (dirname (dirname (current-filename))) "mutt" "muttrc"))
 (cond ((string=? domainname "lysator.liu.se") mutt-global-lysator)
       (else mutt-global-gpg))
 account-list)
