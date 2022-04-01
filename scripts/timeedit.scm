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
        (else (error "Invalid period type"))))

(define-public (scramble string)
  (read-line (open-pipe* OPEN_READ "bin/te_scramble" "--scramble" string)))

(define-public urlbase "https://cloud.timeedit.net/liu/web/schema")

(define*-public (getIdent search #:key (type 219))
  (define url (format #f "~a/objects.json?~a"
                      urlbase
                      (encode-query-parameters
                        `((l . "sv_SE")
                          (search_text . ,search)
                          (types . ,type)))))
  (define-values (headers port)
    (http-get url #:streaming? #t))
  (-> (json->scm port)
      (assoc-ref "records")
      (array-ref 0)
      (assoc-ref "identVirtual")))
