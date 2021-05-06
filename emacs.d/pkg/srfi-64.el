;;; srfi-64.el --- SRFI 64 report mode
;;; Commentary:
;;; Code:

(defvar log-mode-highlights
      `((,(regexp-opt '("pass")) . 'success)
        (,(regexp-opt '("fail")) . 'error))
      )

;; M-x describe-face
;;;###autoload
(define-derived-mode srfi-64-log-mode fundamental-mode "Log"
  "Major mode for viewing srfi-64 log files from guile."

  ;; fold cases
;; Test begin:
;;   source-file: "/home/hugo/code/calparse/tests/termios.scm"
;;   source-line: 34
;;   source-form: (test-equal (& ifl (~ (|| ECHO ICANON))) (lflag t))
;; Test end:
;;   result-kind: pass
;;   actual-value: 0
;;   expected-value: 0
  ;; folds to
  ;; PASS <name if name> <source-file : source-line> ...

  ;; success ration pinned to top of screen
  ;; parsed from end of file
;; # of expected passes      22
;; # of unexpected failures  4

  ;; Failing values should highlight their difference

  ;; Groups should be marked/indented?


  (read-only-mode 1)
  (setq font-lock-defaults '(log-mode-highlights))
  (message "Entered Log mode"))

(provide 'srfi-64)
;;; srfi-64.el ends here

