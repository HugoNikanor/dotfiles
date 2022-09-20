#!/usr/bin/guile \
--no-auto-compile -s
!#

(define here (dirname (current-filename)))

(add-to-load-path here)
(add-to-load-path "/home/hugo/code/calp/module")

(chdir here)

(use-modules (conf-base)
             (util)
             (list)
             ((vdirsyncer) #:prefix vdirsyncer:)
             ((timeedit) #:prefix timeedit:)
             (files)
             (datetime)
             ((hnh util) #:select (->)))

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
          (url ,(format #f "https://calendar.google.com/calendar/ical/~a%40group.calendar.google.com/public/basic.ics"
                        ;; TODO rename this to `calendar-id'?
                        (? url-fragment)))))


(account timeedit (http)
         (search ,(? acc-name))
         (timeedit-objects ,(let ((s (? search)))
                              (if (list? s)
                                (string-join (map timeedit:getIdent s) ",")
                                (timeedit:getIdent s))))
         (timeedit-parameters
           ,`((sid . 3)
              (p . ,(timeedit:parse-period (? period)))
              (objects . ,(? timeedit-objects))
              ))

         ;; "0.n,12.n" From 0 months out to 12 months out
         ;; n - month
         ;; w - week
         ;; d - day
         ;; h - hour
         (period ,(list (start-of-year (current-date))
                        (-> (current-date)
                            start-of-year
                            (date+ (date #:year 1))
                            (date- (date #:day 1)))))

         (remote
           (url ,(format #f "~a/~a.ics"
                         timeedit:urlbase
                         (timeedit:scramble
                           (timeedit:encode-query-parameters
                             (? timeedit-parameters)
                             ))))))

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

(account STABEN2021 (gcal)
         (url-fragment "c_83pc34unvpq53cdm9coui8iunc")
         (color "8B0000"))


(account nolle_p_2020_fadder (gcal)
         (url-fragment "ad7hu028orn8oi1me3aq52nako")
         (color "8B0000"))


(account nolle_p_2020_klassfadder (gcal)
         (url-fragment "c_fblkkscl914d3i2297f82uq00c")
         (color "ADFF2F"))

(account nolle_p_2021_fadder (gcal)
         (url-fragment "c_hlc989be6glfs0k1tleaojirrs")
         (color "70BD44"))


(account lintek (gcal)
         (color "E1007A")
         (url-fragment "lintek.liu.se_jcf23946cujnbq9utv89g7fhcc"))

(account Facebook (http)
         (color "4267B2")
         (remote (url "https://www.facebook.com/events/ical/upcoming/?uid=100000482656459&key=8hYDDImKk9Uh7tRO")))


(account lithekod (gcal)
         (color "1ec6ba")
         (url-fragment "lithekod.se_fa4msgl7qpmsdns5oc4fq48ago"))

(account lithekod_styrelse (gcal)
         (url-fragment "lithekod.se_eos416am56q1g0nuqrtdj8ui1s"))

(account formulastudent (gcal)
         (url-fragment "liuformulastudent.se_0v9ert2schbh487u6hi42imdug"))

(account formulastudent_management (gcal)
         (url-fragment "liuformulastudent.se_ls8331n8jpo570ilur31ig3vq0"))


(account TDDI41_TDP031 (http)
         ;; (period "0.n,12.n")
         ;; (search ,'("TDDI41" "TDP031"))
         (color "BA55D3")
         (remote (url "https://cloud.timeedit.net/liu/web/schema/ri6Y4X96Q6wZ80QwX3042Q35yYYn1Z6Z9425459Q167.ics")))

; flervariabel
(account TATA76 (timeedit))

(account TDDD20 (timeedit))

(account TDDD98 (timeedit))

(account TDDD14 (timeedit)
         (color "5500FF"))

(account TFYA93 (timeedit))

;; statistik
(account TAMS42 (timeedit))

(account TATA42 (timeedit)
         ;; explicit object (instead of search result) since I need
         ;; exact instance
         (timeedit-objects "677801.219")
         (color "FF4D00"))

(account d_sektionen (http)
         (color "754022")
         (remote
           (url "http://kalender.d-sektionen.se")))

(account alma (http)
         (remote
          (url "http://www.lysator.liu.se/alma/alma.cgi?vcal_0=yes&vcal_1=yes&year=2020&vcal_nodefaults=yes&vcal_generate=Ladda+ner+vCalendar-fil")))

;; TODO
;; Make required fields for these more apparent (in their parents)
(account fruux (caldav)
         (color "1E90FF")
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
         (color "ffc0cb")
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


(define calendars
  (list
     ;; STABEN2020
     ;; STABEN2021

     formulastudent
     formulastudent_management

     ;; STABEN
     Facebook
     lithekod
     ;; lithekod_styrelse
     fruux

     TDDI41_TDP031
     ;; TATA76
     ;; TDDD20
     ;; TDDD98
     ;; TDDD14
     ;; TAMS42
     ;; TATA42
     ;; TFYA93

     d_sektionen
     ;; admittansen

     alma
     lintek

     ;; nolle_p_2020_fadder
     ;; nolle_p_2020_klassfadder
     nolle_p_2021_fadder
     ))


(apply ensure-files destdir calendars)

(with-output-to-file path
  (lambda ()
    (apply  vdirsyncer:render
     cal-top destdir
     calendars)))


;; TODO enable systemd timers?

(with-output-to-file
  (path-append destdir (xdg-config-home) "/vdirsyncer/calendars")
  (lambda () (display-list (account-names calendars))))
