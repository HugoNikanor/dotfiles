(define-module (list)
  #:use-module (conf-base)
  #:export (account-name account-names display-list))

(define (account-name account)
  (get-field (instanciate account) '(acc-name)))

(define (account-names accounts)
  (map account-name accounts))

(define* (display-list list #:optional (port (current-output-port)))
  (for-each (lambda (item)
              (display item port)
              (newline port))
            list))
