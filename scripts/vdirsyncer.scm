(define-module (vdirsyncer)
  #:use-module (conf-base)
  #:export (render))

(define (->str v)
  (cond
   ((null? v) "null")
   ((boolean? v) (if v "true" "false"))
   ((symbol? v) (symbol->string v))
   ((string? v) (string-append "\"" v "\""))
   ((list? v) (string-append "[" (string-join (map ->str v) ", ") "]"))
   (else v)))

(define (ini-fmt o path)
  (map-subtree o path
               (lambda (name value)
                 (format #t "~a = ~a~%"
                         name (->str value)))))

(define (render cal-top . accounts)
  (let ((o (instanciate cal-top)))
    (mkdir-p (get-field o '(cal-base)))
    (format #t "[general]~%")
    (ini-fmt o '(general)))

  (for-each
   (lambda (acc)
     (let ((o (instanciate acc)))
       (format #t "~%~%[pair ~a]~%" (get-field o '(acc-name)))
       (ini-fmt o '(pair))

       (format #t "~%[storage ~a]~%" (get-field o '(pair a)))
       (ini-fmt o '(remote))

       (format #t "~%[storage ~a]~%" (get-field o '(pair b)))
       (ini-fmt o '(local))))
   accounts))

