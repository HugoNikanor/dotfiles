(define-module (util)
  #:export (pass escape pass/escape xdg-config-home)
  #:use-module ((conf-base) #:select (path-append))
  #:use-module (ice-9 popen)
  #:use-module (ice-9 rdelim)
  )

(define (pass path-str)
  (let ((pipe (open-input-pipe (string-append "pass " path-str))))
    (let ((return (read-line pipe)))
      (close-pipe pipe)
      return)))



(define (escape cs str)
  (with-output-to-string
    (lambda ()
      (string-for-each
       (lambda (c)
         ;; Always escape the escape char
         (if (char-set-contains? (char-set-adjoin cs #\\) c)
             (display (string #\\ c))
             (display c)))
       str))))

(define* (pass/escape path #:optional (charset (char-set #\")))
  (format #f "\"~a\"" (escape charset (pass path))))


(define (xdg-config-home)
  (or (getenv "XDG_CONFIG_HOME")
      (path-append (getenv "HOME") "/.config")))
