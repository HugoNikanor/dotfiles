(define-module (util)
  #:export (pass escape)
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
         (if (char-set-contains? cs c)
             (display (string #\\ c))
             (display c)))
       str))))
