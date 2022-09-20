(define-module (timeedit))

(add-to-load-path "/home/hugo/code/calp/module")
(use-modules 
             (web client)
             ((hnh util) :select (->))
             (json)
             (ice-9 optargs)
             (ice-9 rdelim)
             (ice-9 popen)
             (datetime)
             )

(define-public (encode-query-parameters parameters)
  (string-join
   (map (lambda (p) (format #f "~a=~a" (car p) (cdr p)))
        parameters)
   "&"))

(define-public (parse-period period)
  (cond ((list? period) (string-join (map parse-period period) "," 'infix))
        ((number? period) (format #f "~a.d" period))
        ((date? period) (date->string period "~Y~m~d.x"))
        ((string? period) period)
        (else (error "Invalid period type"))))

(define-public (scramble string)
  (read-line (open-pipe* OPEN_READ "bin/te_scramble" "--scramble" string)))

(define-public urlbase "https://cloud.timeedit.net/liu/web/schema")

(define*-public (getIdent search #:key (type 219))
  (define url (format #f "~a/objects.json?~a"
                      urlbase
                      (encode-query-parameters
                        `((l . "sv_SE")
                          (max . 100)
                          (fr . t)
                          (partajax . t)
                          (im . f)
                          (sid . 3)
                          (fe . "132.0")
                          (fe . "115.20221,20222")
                          (objects . "695845.219")
                          (search_text . ,search)
                          (types . ,type)))))
  (define-values (_ port)
    (catch 'gnutls-not-available
           (lambda () (http-get url #:streaming? #t))
           (lambda (err msg)
             (format (current-error-port)
                     "~a~%Falling back to curl~%"
                     msg)
             (values #f (open-pipe* OPEN_READ "curl" url)))))
  (-> (json->scm port)
      (assoc-ref "records")
      (array-ref 0)
      (assoc-ref "identVirtual")))
