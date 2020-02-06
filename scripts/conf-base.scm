(define-module (conf-base)
  #:export (account get-field instanciate path-append
                    map-subtree mkdir-p
                    string-first string-last
                    ignore-error)
  #:replace (let*))

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
             (if (string-null? s)
                 (string-append s "/")
                 (if (char=? #\/ (string-last done))
                     (if (char=? #\/ (string-first s))
                         (string-drop s 1) s)
                     (if (char=? #\/ (string-first s))
                         s (string-append "/" s))))))
        (let ((s (car strings)))
          (if (string-null? s)
              "/" s))
        (cdr strings)))


(define (mkdir-p path)
  (reduce (lambda (part done)
            (let ((path (path-append done part)))
              (if (file-exists? path)
                  (let ((st (stat path)))
                    (unless (eq? 'directory (stat:type st))
                      (error "File ~a exists as non-directory" path)))
                  (mkdir path))
              path))
        "" (string-split path #\/)))

;; Replace let* with a version that can bind from lists.
;; Also supports SRFI-71 (extended let-syntax for multiple values)
;; @lisp
;; (let* ([a b (values 1 2)]               ; @r{SRFI-71}
;;        [(c d) '(3 4)]                   ; @r{Let-list (mine)}
;;        [(a b . c) (cons* 1 2 3)]        ; @r{Improper list matching (mine)}
;;        [e 5])                           ; @r{Regular}
;;   (list e d c b a))
;; ;; => (5 4 3 2 1)
;; @end lisp
;;
;; Borrowed from calparse. TODO begin manage scheme libraries again
(define-syntax let*
  (syntax-rules ()

    ;; Base case
    [(_ () body ...)
     (begin body ...)]

    ;; (let (((a b) '(1 2))) (list b a)) => (2 1)
    [(_ (((k ... . (k*)) list-value) rest ...)
        body ...)
     (apply (lambda (k ... k*)
              (let* (rest ...)
                body ...))
            list-value)]

    ;; Improper list matching
    ;; (let* (((a b . c) (cons* 1 2 3))) (list a c)) ; => (1 3)
    [(_ (((k1 k ... . k*) imp-list) rest ...)
        body ...)
     (apply (lambda (k1 k ... k*)
              (let* (rest ...)
                body ...))
            (improper->proper-list
             imp-list (length (quote (k1 k ...)))))]

    ;; "Regular" case
    [(_ ((k value) rest ...) body ...)
     (let ((k value))
       (let* (rest ...)
         body ...))]

    ;; SRFI-71 let-values
    [(_ ((k k* ... values) rest ...) body ...)
     (call-with-values (lambda () values)
       (lambda (k k* ...)
         (let* (rest ...)
           body ...)))]

    ;; Declare variable without a value (actuall #f).
    ;; Useful for inner mutation.
    [(_ (v rest ...) body ...)
     (let* ((v #f) rest ...) body ...)]
    ))

(define (improper->proper-list lst len)
  (let* ((head tail (split-at lst len)))
    (append head (list tail))))



(define-syntax-rule (ignore-error body ...)
  (catch 'misc-error
    (lambda () body ...)
    (lambda _ *unspecified*)))



(define (get-field self field)
  (let inner ((subtree self)
              (subfield field))
    (cond [(null? subtree)  (error "Field not in tree")]
          [(null? subfield) subtree]
          [else (aif (assoc-ref subtree (car subfield))
                     (let ((value (car it)))
                       (if (procedure? value)
                           (value self)
                           (inner value (cdr subfield))))
                     (error "Field contains bad data" self field subfield))])))

(define (map-subtree o subtreepath proc)
  (map (match-lambda [(name field-proc)
                      (proc name (field-proc o))])
       (get-field o subtreepath)))


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

