(define-module (conf-base)
  #:export (account get-field instanciate)
  )

(use-modules (srfi srfi-1)
             (ice-9 match))

(define ($ f x) (f x))

(define (symbol<=? a b)
  (string<=?
    (symbol->string a)
    (symbol->string b)))

;; new-list, old-list -> merged list
(define alist-merge
  (match-lambda*
    [(() ()) '()]
    [(xs ()) xs]
    [(() xs) xs]
    [(((ka . va) as ...) ((kb . vb) bs ...))
     (cond [(eq? ka kb)
            (if (list? (car va))
                (acons ka (alist-merge va vb) (alist-merge as bs))
                (acons ka va (alist-merge as bs)))]
           [(symbol<=? ka kb)
            (acons ka va
                   (alist-merge as (acons kb vb bs)))]
           [else
             (acons kb vb
                    (alist-merge (acons ka va as) bs))])]))


(define* (sort* list < #:key (get identity))
  (sort list (lambda (a b)
               (< (get a)
                  (get b)))))

(define-syntax aif
  (lambda (stx)
    (syntax-case stx ()
      [(_ condition true-clause false-clause)
       (with-syntax ((it (datum->syntax stx 'it)))
         #'(let ((it condition))
             (if it true-clause false-clause)))])))


;; Names stay bound even when the procedure leaves its scope?
(define-syntax-rule (name-procedure name proc)
  (let ((name proc))
    name))



(define (get-field self field)
  (let ((field (if (not (symbol? field))
                   field (list 'toplevel field))))
    (aif (assoc-ref self (car field))
         (aif (assoc-ref it (cadr field))
              ((car it) self)
              (error "Missing subfield" field))
         (error "Missing field" field))))


(define-syntax account
  (lambda (stx)
    (syntax-case stx ()
      [(_ name (parent ...) (key (subkey subvalue) ...) ...)
       (with-syntax ((? (datum->syntax stx '?)))
         #'(define name
             (let ((new-data
                    (sort*
                     `((key ,@(sort*
                               `((subkey
                                  ,(name-procedure subkey
                                     (lambda (self)
                                       (let-syntax
                                           ((? (syntax-rules ()
                                                 [(? symb) (get-field self (quasiquote symb))])))
                                         (quasiquote subvalue))))) ...)
                               symbol<=? #:get car)) ...)
                     symbol<=? #:get car)))
               (name-procedure name
                (lambda (mergable)
                  (alist-merge mergable
                               (fold $ new-data
                                     (list parent ...))))))))])))

;; (define-syntax inner
;;   (lambda (stx)
;;     (syntax-case stx ()
;;       [(_) #'(*endcap* #f)]

;;       [(_ (key (subkey subvalue) ...) rest ...)
;;        #'`((key (inner subkey subvalue) ...)
;;            (inner rest ...))]

;;       [(_ (key value) rest ...)
;;        (with-syntax ((? (datum->syntax stx '?)))
;;          #'`((key (lambda (self)
;;                     (let-syntax ((? (syntax-rules () [(? symb) (get-field self symb)])))
;;                       value)))
;;              (inner rest ...)))]

;;       [(_ key value)
;;        (with-syntax ((? (datum->syntax stx '?)))
;;          #'`(key (lambda (self)
;;                   (let-syntax ((? (syntax-rules () [(? symb)
;;                                                         (get-field self symb)])))
;;                         value))))]))

;;   #;
;;  `((key ,@(sort*
;;            `((subkey
;;               ,(name-procedure subkey
;;                                (lambda (self)
;;                                  (let-syntax
;;                                      ((? (syntax-rules ()
;;                                            [(? symb) (get-field self (quasiquote symb))])))
;;                                    (quasiquote subvalue))))) ...)
;;            symbol<=? #:get car)) ...))

;; (define-syntax account
;;   (lambda (stx)
;;     (syntax-case stx ()
;;       [(_ name (parent ...) values  ...)
;;        (with-syntax ((? (datum->syntax stx '?)))
;;          #'(define name
;;              (let ((new-data
;;                     (sort*
;;                      (inner values ...)
;;                      symbol<=? #:get car)))
;;                (name-procedure name
;;                 (lambda (mergable)
;;                   (alist-merge mergable
;;                                (fold $ new-data
;;                                      (list parent ...))))))))])))


(define (instanciate class . args)
  (class (sort* (alist-merge `((toplevel
                                (acc-name
                                 ,(lambda _ (symbol->string (procedure-name class))))))
                             args)
                symbol<=? #:get car)))
