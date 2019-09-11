(define-module (conf-base)
  #:export (account get-field instanciate path-append
                    string-first string-last)
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
    [(((ka va) as ...) ((kb vb) bs ...))
     (cond [(eq? ka kb)
            (if (and (list? va) (list? vb))
                (acons ka (list (alist-merge va vb)) (alist-merge as bs))
                (acons ka (list va) (alist-merge as bs)))]
           [(symbol<=? ka kb)
            (acons ka (list va) (alist-merge as (acons kb (list vb) bs)))]
           [else
             (acons kb (list vb)
                    (alist-merge (acons ka (list va) as) bs))])]))


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


(define (string-last s)
  (string-ref (string-reverse s) 0))

(define (string-first s)
  (string-ref s 0))


(define (path-append . strings)
  (fold (lambda (s done)
          (string-append
            done
            (if (char=? #\/ (string-last done))
                (if (char=? #\/ (string-first s))
                    (string-drop s 1)
                    s)
                (if (char=? #\/ (string-first s))
                    s
                    (string-append "/" s)))))
        (car strings)
        (cdr strings)))





(define (get-field self field)
  (let inner ((subtree self)
              (subfield field))
    (cond [(null? subtree)  (error "Field not in tree")]
          [(null? subfield) (error "Found subtree")]
          [else (aif (assoc-ref subtree (car subfield))
                     (let ((value (car it)))
                       (if (procedure? value)
                           (value self)
                           (inner value (cdr subfield))))
                     (error "Field contains bad data" self field subfield))])))


(define (instanciate class . args)
  (class (sort* args symbol<=? #:get car)))

(define-syntax inner
  (syntax-rules (unquote)
    [(_ ? (unquote value))
     (lambda (self)
       (let-syntax ((? (with-ellipsis
                        .. (syntax-rules ()
                             [(? path ..) (get-field self `(path ..))]))))
         value))]
    [(_ ? (key sub ...) ...)
     (sort* `((key ,(inner ? sub ...)) ...)
            symbol<=? #:get car) ]
    [(_ _ v v* ...)
     (lambda _ (values `v `v* ...))]))


(define-syntax account
  (lambda (stx)
    (syntax-case stx ()
      [(_ name (parent ...) (key value ...) ...)
       (with-syntax ((? (datum->syntax stx '?)))
         #'(define name
             (let ((new-data (inner ? (acc-name ,(symbol->string 'name))
                                    (key value ...) ...)))
               (lambda (mergable)
                 (alist-merge mergable
                              (fold $ new-data
                                    (list parent ...)))))))])))


