#!/usr/bin/guile \
-e main -s
!#

;;; TODO place everything in proper order 
;;; TODO and document everything

(use-modules (ice-9 format)
             (ice-9 match)
             (ice-9 rdelim)
             (srfi srfi-1)              ; List comprehension
             (srfi srfi-8)              ; Receive
             (srfi srfi-9)              ; Records
             (srfi srfi-26)             ; Cut
             (srfi srfi-69)             ; Hash Tables 
             )

(add-to-load-path "/home/hugo/lib/guile")
(add-to-load-path "/home/hugo/code/guile-fmt")
(use-modules (fmt))

;;; === Required utils ===

(define (->string a)
  "Converts an object to a string.
Really speciallized for this use case. "
  (cond ((string? a) (format #f "~s" a))
        ((list? a) (format #f "[ ~a ]"
                           (string-join (map ->string a)
                                        ", " 'infix)))
        ((number? a) (format #f "~d" a))
        ((symbol? a) (format #f "\"~a\"" a))
        (else (throw 'not-stringable-exception a))))

(define (nlist->string args)
  "Turns an args list into a string"
  (string-join (map ->string args)
               " " 'infix))

(define (set-up-cal calendar defaults)
  "Creates a hash map from a list. Mostly creates strings from all cdr's."
  (let ((table (hash-table-copy defaults)))
    (match calendar
      ((name type . args)
       (for-each (match-lambda
                   ((name ('quote arg))
                    (hash-table-set! table name arg))
                   ((name . args)
                    (hash-table-set! table name (nlist->string args))))
                 `((name ,name) (type ,type)
                   ,@args))))
    table))

(define *defaults*
  (set-up-cal
   '(defaults http
      (metadata (color displayname))
      (conflict_resolution (command vimdiff))
      (url "http://example.com")        ;
      (path "~/.calendars")
      (fileext ".ics")
      (collections null)
      (itype filesystem))
   (make-hash-table)))

;;; === Useful procedures ===

(define (display-table table)
  (hash-table-walk table
                   (lambda (name str)
                     (format #t "~a = ~a~%"
                             name str))))

;;; === Main ===

(define (display-file-if-exists file)
  (when (file-exists? file)
    (display (with-input-from-file file read-string))))

(define (main args)

  (display-file-if-exists "config.pre")
 
  (with-input-from-file "conf.lisp"
    (lambda ()
      (let ((defaults (set-up-cal (read)
                                  (make-hash-table))))
        (receive (head calendars)
            (car+cdr (read))
          (for-each (compose display format-block)
                    (map (cut set-up-cal <> defaults)
                         calendars))))
      (display-file-if-exists "config.post")
      )))

;;; ===

(define-syntax-rule (and/else=> value proc default)
  "Like and=>, but allows #f to be replaced"
  (let ((res value))
    (if res
        (proc res)
        default)))

(define-syntax-rule (str=> value proc)
  "Like and=>, but returns the empty string if value is #f"
  (and/else=> value proc ""))

(define (remove-quotes str)
  "Removes the first and last character of a given string.
Sending strings shorter than 2 is considered an error."
  (substring str 1 (1- (string-length str))))

(define (string-empty? str)
  (string=? str ""))

(define (fmt-keys* ht args)
  (string-join
   (filter (negate string-empty?)
           (map (lambda (key)
                  (str=> (hash-table-ref/default ht key #f)
                         (cut format #f "~a = ~a" key <>)))
                args))
  (format #f "~%")
  'infix))

(define-syntax-rule (fmt-keys ht arg ...)
  (fmt-keys* ht (list (quote arg) ...)))

(define (format-block ht)
  (let ((get (cut hash-table-ref ht <>)))
    (let ((pwcommand
           (str=> (hash-table-ref/default ht 'pass #f)
                  (cut format #f
                       "password.fetch = [\"command\", \"pass\", ~a ]"
                       <>)))
          (name (remove-quotes (get 'name))))
      (fmt "
# ========================================

[pair ${name}]
a = \"${name}_local\"
b = \"${name}_remote\"
${(fmt-keys ht collections metadata conflict_resolution)}

[storage ${name}_local]
${(fmt-keys ht path fileext)}
type = ${(get 'itype)}

[storage ${name}_remote]
${(fmt-keys ht type url username read_only)}
${pwcommand}
"))))



