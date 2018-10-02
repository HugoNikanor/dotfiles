;;; Defaults as a separate SEXP

(defaults http
    (metadata (color displayname))
  (conflict_resolution (command vimdiff))
  (path "~/.calendars")
  (fileext ".ics")
  (collections 'null)
  (itype filesystem))

(calendars
 (fruux caldav
        (collections ("from b"))
        (url "https://dav.fruux.com")
        (username "b3297465009")
        (pass "fruux.com/hugo.hornquist@gmail.com/vdirsyncer/b3297465009"))

 (D1 http
     (conflict_resolution "a wins")
     (url "https://se.timeedit.net/web/liu/db1/schema/ri687Q7QYn4ZQ1Q531650976yZZQ6205.ics")
     (read_only 'true))

 (D2 http
     (conflict_resolution "a wins")
     (url "https://se.timeedit.net/web/liu/db1/schema/ri687Q7QYn4ZQ1Q538650976yZZQ6305.ics")
     (read_only 'true))

 (D3 http
     (conflict_resolution "a wins")
     (url "https://cloud.timeedit.net/liu/web/schema/ri607Q7QYn5ZQ2Q532850976yZZQ6204.ics")
     (read_only 'true)))
