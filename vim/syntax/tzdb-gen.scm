#!/usr/bin/guile -s
!#

(use-modules (hnh util)
             (hnh util env)
             (datetime))

(define (regex-word inner)
  (string-append "\\<" inner "\\>"))

(define (regex-group items)
  (string-append
   "\\%("
   (string-join items "\\|" 'infix)
   "\\)"))

(define (generate-rxs name)
 (let ((head (string-take name 3))
       (tail (string-drop name 3)))
   (string-append
    head
    (regex-group
     (map (lambda (i) (string-take tail i))
          (iota (1+ (string-length tail))))))))

(define month-names
 (with-locale1
  LC_TIME "C"
  (lambda ()
    (regex-word
     (regex-group
      (map (lambda (m)
             (-> (date month: m)
                 (date->string "~B")
                 generate-rxs))
           (iota 12 1)))))))

(define weekday-names
 (with-locale1
  LC_TIME "C"
  (lambda ()
    (regex-word
     (string-append "\\%(last\\)\\?"
                    (regex-group
                     (map (lambda (d)
                            (-> (date+ #2000-01-01 (date day: (modulo d 7)))
                                (date->string "~A")
                                generate-rxs))
                          (iota 7))))))))

(format #t "syntax match Constant /~a/~%" month-names)
(format #t "syntax match Constant /~a/~%" weekday-names)
