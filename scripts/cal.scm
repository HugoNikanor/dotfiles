#!/usr/bin/guile \
--no-auto-compile -s
!#

(add-to-load-path (dirname (current-filename)))

(use-modules (conf-base)
             (util)
             ((vdirsyncer) #:prefix vdirsyncer:)
             (files))

;; http://vdirsyncer.pimutils.org/en/stable/config.html

(define static-passwords
  (getenv "STATIC_PASSWORD"))
(define destdir (or (getenv "DESTDIR") "/"))

(account cal-top ()
         (prefix ,(or (getenv "PREFIX")
                      (getenv "HOME")))
         (static-passwords ,(getenv "PREFIX"))
         (cal-base ,(path-append (? prefix) ".local/var/cal"))
         (vdirsyncer-config ,(path-append (? prefix) "/.config/vdirsyncer/config"))
         (general (status_path
                   ,(path-append (? prefix)
                                 ".local/share/vdirsyncer/status/")))
         )

(account cal (cal-top)
         (pair (a ,(format #f "~a_remote" (? acc-name)))
               (b ,(format #f "~a_local" (? acc-name)))
               (collections null)
               (conflict_resolution "a wins"))
         #; (remote)
         (local
          (type "filesystem")
          (fileext ".ics")
          (path ,(path-append (? cal-base) (? acc-name))))
         (files
           (color (path ,(path-append (? local path) "color"))
                  (content ,(string-append "#" (? color)))))
         )

(account http (cal)
         (pair
           (partial_sync "ignore"))
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

(account caldav (cal)
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
          (path ,(path-append (? cal-base) (? acc-name))))

         (remote
          (type "caldav"))

         )

;; pacman -S python-requests-oauthlib
;; pip3 install --user requests-oauthlib
(account google (caldav)
         (pair (metadata ,'("displayname")))
         (remote (type "google_calendar")
                 (token_file ,(path-append
                                (? prefix)
                                "/.cache/vdirsyncer-tokens"))))






(account STABEN2019 (gcal)
         (url-fragment "d.lintek.liu.se_90a0j5e3r9oc6dotfbe716n8ns"))

(account STABEN2020 (http)
         (remote (url
                   "https://backend.staben.info/info/calendar/d0c1"))
         (color "FF0000"))


(account nolle_p_2020_fadder (gcal)
         (url-fragment "ad7hu028orn8oi1me3aq52nako")
         (color "8B0000"))


(account nolle_p_2020_klassfadder (gcal)
         (url-fragment "c_fblkkscl914d3i2297f82uq00c")
         (color "ADFF2F"))


(account lintek (gcal)
         (url-fragment "lintek.liu.se_jcf23946cujnbq9utv89g7fhcc"))

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
         (color "FFA500")
         (url-fragment 
           "ri687Q7QYn4ZQ1Q502860976yZZQ6203"
           ))

(account D2 (timeedit)
         (url-fragment "ri687Q7QYn4ZQ1Q538650976yZZQ6305"))

(account D3 (timeedit)
         (url-fragment "ri607Q7QYn5ZQ2Q532850976yZZQ6204"))

(account TDDI41_TDP031 (timeedit)
         (url-fragment "ri634Q36Y87Z90Q5Y7887QX9y2Zn660Z45Q6524Q637"))

(account TSTE24 (timeedit)
         (url-fragment "ri664XQn580Z55Qm77025ZZ6y9Y740QQ0Y45Y0gQ10764"))

(account TDDE44 (timeedit)
         (url-fragment "ri697Q7QYn6ZQ6Q552865471yZZQ6805"))

(account TATA41 (timeedit)
         (url-fragment "ri660XQn020Z56Qm67035ZZ6y9Y740QQ0Y43Y5gQ60765"))

; flervariabel
(account TATA76 (timeedit)
         (url-fragment "ri687Q7QYn3ZQ6Q551865471yZZQ6805"))

(account TDDB68 (timeedit)              ; pintos
         (url-fragment "ri660XQn020Z56Qm07075ZZ6y9Y740QQ0Y43Y5gQ00765"))

(account d_sektionen (http)
         (remote
          (url "https://calendar.google.com/calendar/ical/webmaster%40d.lintek.liu.se/public/basic.ics")))

(account alma (http)
         (remote
          (url "http://www.lysator.liu.se/alma/alma.cgi?vcal_0=yes&vcal_1=yes&year=2020&vcal_nodefaults=yes&vcal_generate=Ladda+ner+vCalendar-fil")))

;; TODO
;; Make required fields for these more apparent (in their parents)
(account fruux (caldav)
         (pass-path ,(format
                      #f "fruux.com/hugo.hornquist@gmail.com/vdirsyncer/~a"
                      (? remote username)))
         (remote
          (url "https://dav.fruux.com")
          (username "b3297465009")
          ,@(if static-passwords
                ((password ,(pass (? pass-path))))
                ((password.fetch ,`("command" "pass" ,(? pass-path))))
              )
          ))

(account admittansen (google)
         (remote
          (client_id ,(pass "admittansen/google/oauth/client_id"))
          (client_secret ,(pass "admittansen/google/oauth/client_secret"))))

(define path (path-append
               destdir
               (get-field (instanciate cal-top)
                          '(vdirsyncer-config))))

;; should be configurable, but *shrug*
(umask #o077)

(mkdir-p (dirname path))


(ensure-files destdir
              D1 STABEN2020
  nolle_p_2020_fadder
  nolle_p_2020_klassfadder)


(with-output-to-file path
  (lambda ()
    (vdirsyncer:render
     cal-top
     destdir
     ;; D1
     ;; STABEN2020
     ;; TDDE04
     TDDE44
     ;; TSTE24

     formulastudent
     formulastudent_management

     ;; STABEN
     Facebook
     lithekod
     ;; lithekod_styrelse
     fruux

     TDDI41_TDP031
     ;; TATA41
     ;; TDDB68
     ;; TATA76

     d_sektionen
     admittansen

     alma
     lintek

     nolle_p_2020_fadder
     nolle_p_2020_klassfadder
     )))
