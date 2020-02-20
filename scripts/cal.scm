#!/usr/bin/guile \
--no-auto-compile -s
!#

(add-to-load-path (dirname (current-filename)))

(use-modules (conf-base)
             (util)
             ((vdirsyncer) #:prefix vdirsyncer:))

(account cal-top ()
         (cal-base ,(path-append (getenv "HOME") ".local/var/cal"))
         (general (status_path
                   ,(path-append (getenv "HOME")
                                 ".local/share/vdirsyncer/status/"))))

(account cal (cal-top)
         (pair (a ,(format #f "~a_remote" (? acc-name)))
               (b ,(format #f "~a_local" (? acc-name)))
               (collections null)
               (conflict_resolution "a wins"))
         #; (remote)
         (local
          (type "filesystem")
          (fileext ".ics")
          (path ,(path-append (? cal-base) (? acc-name)))))

(account http (cal)
         (remote
          (type "http")
          (read_only true)))

(account gcal (http)
         (remote
          (url ,(format #f "https://calendar.google.com/calendar/ical/~a%40group.calendar.google.com/public/basic.ics" (? url-fragment)))))

(account timeedit (http)
         (remote
          (url ,(format #f "https://cloud.timeedit.net/liu/web/schema/~a.ics"
                        (? url-fragment)))))





(account STABEN (gcal)
         (url-fragment "d.lintek.liu.se_90a0j5e3r9oc6dotfbe716n8ns"))


(account Facebook (http)
         (remote (url "https://www.facebook.com/events/ical/upcoming/?uid=100000482656459&key=8hYDDImKk9Uh7tRO")))


(account lithekod (gcal)
         (url-fragment "lithekod.se_fa4msgl7qpmsdns5oc4fq48ago"))

(account lithekod_styrelse (gcal)
         (url-fragment "lithekod.se_eos416am56q1g0nuqrtdj8ui1s"))

(account formulastudent (gcal)
         (url-fragment "liuformulastudent.se_0v9ert2schbh487u6hi42imdug"))

(account formulastudent_management (gcal)
         (url-fragment "liuformulastudent.se_ls8331n8jpo570ilur31ig3vq0"))

(account TDDE04 (timeedit)
         (url-fragment "ri653Q85Y91Z90Q5Y7607QX9y5Zn614Z45Q2584Q687"))

(account D1 (timeedit)
         (url-fragment "ri677Q7QYn8ZQ9Q540650975yZZQ6805"))

(account D2 (timeedit)
         (url-fragment "ri687Q7QYn4ZQ1Q538650976yZZQ6305"))

(account D3 (timeedit)
         (url-fragment "ri607Q7QYn5ZQ2Q532850976yZZQ6204"))

(account TDDI41_TDP031 (timeedit)
         (url-fragment "ri65X456X46Z04Q5Z96g8Y90y5056Y51905gQY6Q53798580Y24567Q0ZnQ5473"))

(account TSTE24 (timeedit)
         (url-fragment "ri664XQn580Z55Qm77025ZZ6y9Y740QQ0Y45Y0gQ10764"))

(account TDDE44 (timeedit)
         (url-fragment "ri660XQn020Z56Qm77085ZZ6y9Y740QQ0Y43Y5gQ90765"))

(account TATA41 (timeedit)
         (url-fragment "ri660XQn020Z56Qm67035ZZ6y9Y740QQ0Y43Y5gQ60765"))

(account TDDB68 (timeedit)              ; pintos
         (url-fragment "ri660XQn020Z56Qm07075ZZ6y9Y740QQ0Y43Y5gQ00765"))

(account d_sektionen (http)
         (remote
          (url "https://calendar.google.com/calendar/ical/webmaster%40d.lintek.liu.se/public/basic.ics")))


(account fruux (cal)
         ;; 'from a' means to discover from the server
         ;; 'from b' means that calendars are discovered in the local
         ;; dir.
         ;; TODO rename 'a' & 'b' to 'remote' & 'local'?
         (pair (collections ,'("from a"))
               (metadata ,'("color" "displayname"))
               (conflict_resolution ,'("command" "vimdiff")))

         ;; This creates the fruux directory, and discovered
         ;; directories are placed directly below.
         ;; I currently symlink those I want to observe into the base
         ;; directory.
         (local
          (path ,(path-append (? cal-base) "fruux")))

         (remote
          (type "caldav")
          (url "https://dav.fruux.com")
          (username "b3297465009")
          (password.fetch ,`("command" "pass"
                             ,(format #f "fruux.com/hugo.hornquist@gmail.com/vdirsyncer/~a" (? remote username))))))


;; pacman -S python-requests-oauthlib
;; pip3 install --user requests-oauthlib
(account admittansen (cal)
         (pair (collections ,'("from a"))
               (metadata ,'("displayname"))
               (conflict_resolution ,'("command" "vimdiff")))

         ;; see comment on fruux
         (local
          (path ,(path-append (? cal-base) "admittansen")))

         (remote
          (type "google_calendar")

          (token_file "/home/hugo/.cache/vdirsyncer-tokens")
          (client_id ,(pass "admittansen/google/oath/client_id"))
          (client_secret ,(pass "admittansen/google/oath/client_id"))))

(define path (path-append (getenv "HOME") "/.config/vdirsyncer"))
(mkdir-p path)

(with-output-to-file (path-append path "config")
  (lambda ()
    (vdirsyncer:render
     cal-top

     ;; TDDE04
     TDDI41_TDP031
     TDDE44
     ;; TSTE24

     formulastudent
     formulastudent_management

     ;; STABEN
     Facebook
     lithekod
     lithekod_styrelse
     fruux

     TATA41
     TDDB68

     d_sektionen
     admittansen
     )))
