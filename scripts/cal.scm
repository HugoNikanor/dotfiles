#!/usr/bin/guile \
-s
!#

(add-to-load-path (dirname (current-filename)))

(use-modules (conf-base)
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
         (remote (url "https://www.facebook.com/events/ical/upcoming/?uid=100000482656459&key=AQB8kEAx-Ry0CIu2")))


(account lithekod (gcal)
         (url-fragment "lithekod.se_fa4msgl7qpmsdns5oc4fq48ago"))

(account lithekod_styrelse (gcal)
         (url-fragment "lithekod.se_eos416am56q1g0nuqrtdj8ui1s"))

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


(account fruux (cal)
         (pair (collections ,'("from b"))
               (metadata ,'("color" "displayname"))
               (conflict_resolution ,'("command" "vimdiff")))

         #;
         (local
          (path ,(? cal-base)))

         (remote
          (type "caldav")
          (url "https://dav.fruux.com")
          (username "b3297465009")
          (password.fetch ,`("command" "pass"
                             ,(format #f "fruux.com/hugo.hornquist@gmail.com/vdirsyncer/~a" (? remote username))))))



(define path (path-append (getenv "HOME") "/.config/vdirsyncer"))
(mkdir-p path)

(with-output-to-file (path-append path "config")
  (lambda ()
    (vdirsyncer:render
     cal-top

     TDDE04
     TDDI41_TDP031
     TSTE24

     STABEN
     Facebook
     lithekod
     lithekod_styrelse
     fruux
     )))