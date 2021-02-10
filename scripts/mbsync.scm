(define-module (mbsync)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:use-module (conf-base)
  #:export (render category-transformer))




(define (category-transformer category name)
  (string-append
   name
   (case category
     [(MaildirStore) "-local"]
     [(IMAPStore)   "-remote"]
     [else ""]
     )))

(define (render-mbsync-account account)
  (let ((obj (instanciate account)))
    (mkdir-p (get-field obj '(MaildirStore Path)))
    (for-each
     (lambda (category)
       (format #t "~%~a ~a~%" category (category-transformer
                                        category
                                        (get-field obj '(acc-name))))
       (for-each
        (match-lambda ((key proc)
                       (format #t "~a ~a~%"
                               key (proc obj))))
        (car (assoc-ref obj category))))
     '(IMAPAccount MaildirStore IMAPStore Channel))
    (format #t "# ~a~%" (make-string 40 #\-))
    obj))


(define (render accounts)
  (let ((objects (map render-mbsync-account (remove unspecified? accounts))))
    (format #t "~%Group all~%")
    (for-each (lambda (name) (format #t "Channel ~a~%" name))
              (map (lambda (o) (get-field o '(acc-name))) objects))))
