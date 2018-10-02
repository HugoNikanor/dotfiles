#!/usr/bin/guile \
-e main -s
!#

;;; TODO place everything in proper order 
;;; TODO and document everything

(use-modules (ice-9 format)
             (ice-9 match)
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
  (string-join (map ->string args)
               " " 'infix))

(define (set-up-cal calendar defaults)
  (let ((table (hash-table-copy defaults)))
    (match calendar
      ((name type . args)
       (for-each (match-lambda
                   ((name . args)
                    (hash-table-set! table name (nlist->string args))))
                 `((name ,name) (type ,type)
                   ,@args))))
    table))

#;
(define (hash-copy table)
  "Shallow copies the given hash table into a new hash table."
  (let ((new-table (make-hash-table)))
    ;; Why doesn't cut work here?
    (hash-table-walk table (lambda (key value)
                             hash-table-set! new-table key value))
    new-table))

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

(define (main args)
  (with-input-from-file "conf.lisp"
    (lambda ()
      (let ((defaults (set-up-cal (read)
                                  (make-hash-table))))
        (receive (head calendars)
            (car+cdr (read))
          (for-each (compose display format-block)
                    (map (cut set-up-cal <> defaults)
                         calendars))
          )))))

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

(define (format-block ht)
  (let ((get (cut hash-table-ref ht <>)))
    (let ((pwcommand
           (str=> (hash-table-ref/default ht 'pass #f)
                  (cut format #f "password.fetch = [\"command\", \"pass\", ~a ]" <>)))
          (username (str=> (hash-table-ref/default ht 'username #f)
                           (cut format #f "username = ~a" <>)))
          (name (substring (get 'name)
                           1 (1- (string-length (get 'name))))))
      (fmt "
[pair ${name}]
a = \"${name}_local\"
b = \"${name}_remote\"
collections = ${(get 'collections)}
metadata = ${(get 'metadata)}
conflict_resolution = ${(get 'conflict_resolution)}

[storage ${name}_local]
type = ${(get 'itype)}
path = ${(get 'path)}
fileext = ${(get 'fileext)}

[storage ${name}_remote]
type = ${(get 'type)}
url = ${(get 'url)}
${username}
${pwcommand}
"))))



