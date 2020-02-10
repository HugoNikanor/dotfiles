(define-module (util)
  #:export (pass)
  #:use-module (ice-9 popen)
  #:use-module (ice-9 rdelim)
  )

(define (pass path-str)
  (let ((pipe (open-input-pipe (string-append "pass " path-str))))
    (let ((return (read-line pipe)))
      (close-pipe pipe)
      return)))

