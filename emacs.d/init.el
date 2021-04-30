;;; init.el --- Init file for emacs
;;; Commentary:
;;; Code:

(eval-when-compile (require 'cl))
;; https://a-nickels-worth.blogspot.com/2007/11/effective-emacs.html
(defvar *emacs-load-start* (current-time))
(require 'package)

(setq package-archives
      '(("gnu" . "https://elpa.gnu.org/packages/")
        ("melpa" . "https://melpa.org/packages/")
        ("org" . "https://orgmode.org/elpa/")
        ))

(defvar required-packages)
(setq required-packages
      `(
        ivy                             ; M-x fuzzy finder
        paredit
        popup
        which-key                       ; show possible keys

        smart-tabs-mode

        evil evil-paredit
))


(defvar other-packages)
(setq other-packages
      '(
        flycheck
        haskell-mode
        elm-mode
        markdown-mode
        cider

        magit
        git-gutter
        mmm-mode                            ; Multiple Major Modes

        yasnippet
        yasnippet-snippets

        frames-only-mode

        evil-org

        ;; TODO This errors out in install-all,
        ;; but installing it manually works fine
        irfc-mode

        ;; rainbow-delimiters
        ;; org-plus-contrib
        ))


;;; M-x package-install-selected-packages to actually install
(setq package-selected-packages `(,@ required-packages ,@other-packages))

(package-initialize)

(package-refresh-contents t)

(mapc #'require required-packages)



;;; Local Packages

(add-to-list 'load-path "~/.emacs.d/pkg")

;; Update local autoloads
(require 'autoload)
(setq generated-autoload-file "~/.emacs.d/pkg/my-autoloads.el")
(update-directory-autoloads "~/.emacs.d/pkg")
(require 'my-autoloads)



;;; Defines for making it this file better

(defmacro hook-envs (function environments)
  "Add function to list of hooks.
FUNCTION: function to run for all the hooks
ENVIRONMENTS: all hooks to bind to"
  `(mapc (lambda (hook)
           (add-hook hook ,function))
         ,environments))


;;; Evil

(evil-mode)

(require 'evil-maps)                    ; Is this required?

(setq evil-split-window-below t
      evil-vsplit-window-right t)

;; TODO auto find TAGS file

;;; Multiple problems here.
;;; Defalias evaluates it's arguments when run, so the major-mode
;;; providing an xref backend must be loaded before xref-find-backend
;;; is called. The obvious solution was to wrap xref-backend-... in a
;;; propper function, but that doesn't work since the procedure is run
;;; in minibuffer-inactive-mode.
;;; A function wrapping defalias, along with some hooks is a hack.
(defun def-read-tag-name ()
 (defalias 'read-tag-name
   (xref-backend-identifier-completion-table (xref-find-backend))))

(add-hook 'geiser-mode-hook 'def-read-tag-name)
(add-hook 'emacs-lisp-mode-hook 'def-read-tag-name)

(evil-ex-define-argument-type tag
  "Handles a jump-tag argument"
  :collection read-tag-name)

(evil-define-interactive-code "<tag>"
  :ex-arg tag
  (list (when (evil-ex-p) evil-ex-argument)))

(evil-define-command evil-open-tag (tagname)
  :repeat nil
  (interactive "<tag>")
  (xref-find-definitions tagname))

(evil-ex-define-cmd "ta[g]" 'evil-open-tag)


(evil-define-command evil-vsplit-buffer (buffer)
  "Splits window and switches to another buffer."
  :repeat nil
  (interactive "<b>")
  (evil-window-vsplit)
  (evil-buffer buffer))

(evil-ex-define-cmd "vsb[uffer]" 'evil-vsplit-buffer)
(evil-ex-define-cmd "vsp" "vsplit")



(define-key evil-insert-state-map (kbd "C-n") #'completion-at-point)

(defun evil-fresh-line-below (&optional count)
  "Open-below, followed by returning to normal mode.
COUNT: number of lines to add"
  (interactive "p")
  (evil-open-below count)
  (evil-normal-state))

(defun evil-fresh-line-above (&optional count)
  "Open-above, followed by returning to normal mode.
COUNT: number of lines to add"
  (interactive "p")
  (evil-open-above count)
  (evil-normal-state))

;; evil-want-C-u-scroll does exist, but only works for motion state
(evil-define-key '(normal motion) 'global
  (kbd "C-u") 'evil-scroll-up)

(evil-define-key '(motion) paredit-mode-map
  (kbd "C-k") 'kill-sexp)

;;; [RET] is the "primitive" return code, exactly the same as ^M
;;; <return> is a high level construct, and only available in GUI.
(define-key evil-normal-state-map (kbd "RET") 'evil-fresh-line-below)
(define-key evil-normal-state-map (kbd "<S-return>") 'evil-fresh-line-above)




(ivy-mode)
(which-key-mode) ; Show possible next keys after some key presses
(setq which-key-allow-evil-operators t
      which-key-allow-imprecise-window-fit t)
(show-paren-mode)
(column-number-mode)

(frames-only-mode)

(menu-bar-mode 0)
(tool-bar-mode 0)
(scroll-bar-mode 'right)

;; TODO load this for modes I want it in
;; (yas-global-mode)

;; (defvar og-whitespace-style whitespace-style)
;; Highlight "bad" whitespace.
;; Unfortunately breaks "regular" whitespace mode.
;; (setq whitespace-style
;;       '(face space-before-tab trailing))

;; Mark lines not part of file.
(setq-default indicate-empty-lines t)

(setq-default show-trailing-whitespace t)

(when (functionp #'add-variable-watcher)
  (add-variable-watcher
   'buffer-read-only
   (lambda (symbol newval operation where)
     "symbol is 'buffer-read-only,
operation is in '(set let unlet makeunbound defvaralias),
where is a buffer or nil"
     (setq-local show-trailing-whitespace (not newval)))))

(setq inhibit-startup-screen t)

;; (setq mmm-submode-decoration-level 0)
;; (setq mmm-global-mode 'maybe)



(require 'formfeed)

;; TODO this is't imidieately run for new buffers, instead first updating when
;; the window first changes shape.
(formfeed-mode 1)



(setq-default fill-column 80)
(add-hook 'text-mode-hook #'auto-fill-mode)
;; (add-hook 'tex-mode-hook (lambda () (setq fill-column 60)))

(add-hook 'tex-mode-hook #'yas-minor-mode-on)
(add-hook 'tex-mode-hook #'babel-lang)

(defun insert-text-line (&optional width)
  "Insert a markdown <HR/> tag.
Replaces formfeed-modes ^L
WIDTH: number of dashes in line"
  (interactive "p")
  (insert (make-string (if (= width 1) 40 width) ?-))
  (newline))

(add-hook 'text-mode-hook #'flyspell-mode)
(add-hook 'markdown-mode-hook
          (lambda ()
            (setq fill-column 70)
            (evil-define-minor-mode-key '(normal insert replace) formfeed-mode
              (kbd "C--") 'insert-text-line)))


;;; Whitespace

(setq-default indent-tabs-mode nil)

(smart-tabs-insinuate
 'c 'c++ 'java 'javascript
 'python 'ruby 'cperl 'ruby)

;;; this should make evil-mode work with smart-tabs-mode,
;;; Mostly for `<' & `>' shifting. But it doesn't.
(setq evil-indent-convert-tabs nil)


;;; Org Mode

(defun org-mode-stuff ()
  (evil-define-key 'normal org-mode-map (kbd "z j")
    'org-forward-heading-same-level)
  (evil-define-key 'normal org-mode-map (kbd "z k")
    'org-backward-heading-same-level)
  (setq org-treat-insert-todo-heading-as-state-change t
        org-hide-leading-stars t
        org-agenda-default-appointment-duration 60))

(add-hook 'org-mode-hook #'evil-org-mode)
(add-hook 'org-mode-hook #'org-mode-stuff)


;;; Prettify

(defun prettify-scheme ()
  (setq prettify-symbols-alist
        '(("lambda" . #x3bb)            ; λ
          ("<=" . #x2264)               ; ≤
          (">=" . #x2265)               ; ≥
          ("memv" . ?∈)
          ;; ("sum" . #x2211)              ; ∑
          ;; ("prod" . #x220f)             ; ∏
          )))
(add-hook 'scheme-mode-hook #'prettify-scheme)
(add-hook 'geiser-repl-mode-hook #'prettify-scheme)

(defun prettify-tex ()
  ;; \mathnote{v = p} -> /v = p/ (but with larger slashes)

  ;; \par is currently replaced with something my emacs
  ;; fails to render. I think it's a vertical tab.

  (setq prettify-symbols-alist
        (append
         prettify-symbols-alist
         '(("\\pm" . ?±)
           ("\\eset" . ?∅)
           ("\\union" . ?∪)
           ;; ("\\bunion" . ?⋃)
           ("\\comp" . ?∁)
           ("\\endproof" . ?□)
           ("\\snitt" . ?∩)
           ("\\sqrt" . ?√)
           ("\\left(" . ?\()
           ("\\right)" . ?\))
           ;; COMBINING OVERLINE
           ("\\vec{v}" . "v̅")
           ("\\vec{u}" . "u̅")
           ("\\lto" . ?⇒)               ; leads to
           ("\\gaffla" . ?ψ)
           ("\\Gaffla" . ?Ψ)
           ("\\trassla" . ?ξ)
           ))))

(add-hook 'tex-mode-hook #'prettify-tex)

(defun prettify-elm ()
  (setq prettify-symbols-alist
        (append
         prettify-symbols-alist
         '(("\\" . ?λ)
           ("->" . ?→)
           ("<-" . ?←)
           ))))

(add-hook 'elm-mode-hook #'prettify-elm)

(global-prettify-symbols-mode 1)


;;; Info Mode

(loop for p in '("/home/hugo/.local/share/info" "/usr/local/share/info")
      do (add-to-list 'Info-default-directory-list p))

(defun info-binds ()
  (evil-define-key 'motion Info-mode-map
    "l" 'Info-last
    "N" 'evil-search-next
    "P" 'evil-search-previous))

(add-hook 'Info-mode-hook #'info-binds)


;; (require 'irfc)

(evil-define-key 'normal irfc-mode-map
  "t" 'irfc-head-goto
  "[[" 'irfc-head-prev
  "]]" 'irfc-head-next
  "]x" 'irfc-page-next
  "[x" 'irfc-page-prev
  "j" 'scroll-up-line
  "k" 'scroll-down-line
  (kbd "RET") 'irfc-follow)

(setq irfc-directory "~/.local/doc/rfc/")
(defvar rfc-index-file (concat irfc-directory "rfc-index.txt"))

(defun rfc-list ()
  "Parse `rfc-index-file` for all RFC's.
TODO I should filter out obsoleted matches"
  (let ((string (with-current-buffer
                    (find-file-noselect rfc-index-file)
                  (buffer-string))))
    (save-match-data
      (let ((pos 0)
            matches)
        (while (string-match "^[0-9]+ .*" string pos)
          (push (match-string 0 string) matches)
          (setq pos (match-end 0)))
        (reverse matches)))))


(defun irfc-goto (str)
  "Irfc-visit, with a list of all RFC's.
STR: target string"
  (interactive
   (list (completing-read "Open RFC: " (rfc-list))))
  (irfc-visit (car (read-from-string str))))


;;; Paredit


'(comment ; Something like this should be used insead,
  (use-modules (ice-9 pretty-print))
  (call-with-input-string
   (string-drop "=> (1 (2 (3 (a) b) c))" 3)
   (compose pretty-print
            read)))


(defvar-local *eval-sexp* 'identity)
(defvar-local *eval-sexp-print* 'identity)

(defun eval-sexp-print ()
  "Eval sexp before point and print it."
  (interactive)
  (funcall (symbol-function *eval-sexp-print*)))

(defun eval-sexp ()
  "Eval sexp before point, quietly."
  (interactive)
  (funcall (symbol-function *eval-sexp*)))


(define-key paredit-mode-map (kbd "C-j")
  'eval-sexp-print)

(define-key paredit-mode-map (kbd "C-S-j")
  'eval-sexp)

(add-hook 'paredit-mode-hook #'evil-paredit-mode)

(hook-envs #'enable-paredit-mode
 '(emacs-lisp-mode-hook
   eval-expression-minibuffer-setup-hook
   ielm-mode-hook
   lisp-mode-hook
   lisp-interaction-mode-hook
   scheme-mode-hook
   clojure-mode-hook))



;;; Emacs Lisp

(defun elisp-eval-popup ()
  "Evaluate the elisp sexp before point, and display the result in a popup."
  (interactive)
  (popup-tip
   (with-output-to-string
     (princ "⇒ ")
     (princ (eval (elisp--preceding-sexp))))))

(hook-envs
 (lambda ()
   (setq *eval-sexp-print* 'eval-print-last-sexp
         *eval-sexp*       'elisp-eval-popup))
 '(emacs-lisp-mode-hook
   lisp-interaction-mode-hook))


;;; Common Lisp


(setq-default inferior-lisp-program "sbcl")

(add-hook 'lisp-mode-hook
 (lambda ()
   (require 'slime)
   (setq *eval-sexp-print* 'slime-eval-print-last-expression
         *eval-sexp*       'slime-eval-last-expression)))


;;; Clojure

;; tex-latex-indent-syntax-table

(defun clojure-env ()
  (require 'cider)

  (define-clojure-indent
    (defroutes 'defun)
    (GET 2) (POST 2) (PUT 2)
    (DELETE 2) (HEAD 2) (ANY 2)
    (OPTIONS 2) (PATCH 2) (rfn 2)
    (let-routes 1) (context 2)
    (html5 2)
    (full-page 1)))

(add-hook 'clojure-mode-hook #'clojure-env)


;;; Scheme / Geiser

;;; Geiser seems to not work to well with evaling sexp's directly. I should
;;; however be able to create a new buffer, and eval the above there.

;; =C-u C-u M-x geiser-eval-last-sexp= does this,
;; But without the fancy formatting!
(defun geiser-eval-print-last-sexp ()
  "Eval scheme sexp before point, and prints it to buffer."
  (interactive)
  (let ((ret (geiser-eval-last-sexp nil))
        (cmnt
         (if (= (point)
                (line-beginning-position))
             ";; "
           " ; ")))
    (if (equalp "=> " ret)
        ;; TODO better error handling
        (insert cmnt "EVALUATION ERROR")
      ;; Mark should be returned after
      (set-mark (point))
      (insert cmnt ret)
      (indent-region (+ 5 (mark))
                     (point)))))

(defun geiser-eval-popup-last-sexp ()
  "Eval scheme sexp before point, and pops up the result."
  (interactive)
  (let ((ret (geiser-eval-last-sexp nil)))
    (popup-tip (if (equalp ret "⇒ ")
                   "EVALUATION ERROR"
                 ret))))


(add-hook
 'scheme-mode-hook
 (lambda ()
   (require 'geiser)
   (geiser-mode)

   ;; Let's pretend any scheme buffer is an interaction scheme buffer!
   ;; geiser-eval-last-sexp doesn't like guile reader extensions ("#")
   (setq *eval-sexp-print* 'geiser-eval-print-last-sexp
         *eval-sexp*       'geiser-eval-popup-last-sexp)

   (font-lock-add-keywords
    nil `(,(regexp-opt '("mod!" "set!") 'symbols)
          ("\\<\\w+:\\>" . font-lock-constant-face)
          ("#\\\\\\(.\\w*\\)" . (1 font-lock-string-face))
          ("(\\<\\(define-\\w*\\)\\>\s +(?\\(\\S +\\)?"
           (1 ,font-lock-keyword-face) (2 ,font-lock-function-name-face))))))

(evil-define-key '(normal emacs insert) geiser-repl-mode-map
  (kbd "C-l") 'geiser-repl-clear-buffer)

;; geiser-repl-mode

(add-hook 'geiser-mode-hook
          (lambda ()
            ;; Geiser only looks at these, if this list is here
            (setq geiser-active-implementations '(guile chicken racket)
                  evil-lookup-func #'geiser-doc-symbol-at-point)

            (setq geiser-chicken-binary "chicken-csi")
            (when (string-prefix-p "lysator.liu.se" (shell-command-to-string "hostname -d"))
              (setq geiser-chicken-binary "csi"))

            (setq geiser-guile-load-path '("/home/hugo/lib/guile" "."))
            ;; geiser-guile-extra-keywords
            ;; geiser-guile-init-file
            ;; geiser-guile-load-init-file-p

            (evil-define-key '(normal insert) scheme-mode-map
              (kbd "M-.") 'geiser-edit-symbol-at-point
              (kbd "C-]") 'geiser-edit-symbol-at-point)

            ;; TODO
            ;; /usr/share/emacs/26.3/lisp/progmodes/etags.el.gz
            ;; Sätt upp det här som en xref-backend
            ;; (evil-ex-define-cmd "ta[g]" 'geiser-edit-symbol)


            ;; extend theme to show comments and comment
            ;; markers in different colors.
            (mmm-add-classes
             '((lisp-texinfo-comments
                :submode texinfo-mode
                :front "^;; "
                ;; :back "^[^;]"
                :back "$"
                :include-front nil
                :include-back nil
                )))

            (mmm-add-mode-ext-class
             'scheme-mode nil 'lisp-texinfo-comments)))




(setq log-mode-highlights
      `((,(regexp-opt '("pass")) . 'success)
        (,(regexp-opt '("fail")) . 'error))
      )

  ;; M-x describe-face
(define-derived-mode log-mode fundamental-mode "Log"
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

(add-to-list 'auto-mode-alist '("\\.log\\'" . log-mode))

;;; Haskell

(add-hook
 'haskell-mode-hook
 (lambda ()
   (mmm-add-classes
    '((literate-haskell-bird
       :submode text-mode
       :front "^[^>]"
       :include-front true
       :back "^>\\|$")
      (literate-haskell-latex
       :submode literate-haskell-mode
       :front "^\\\\begin{code}"
       :front-offset (end-of-line 1)
       :back "^\\\\end{code}"
       :include-back nil
       :back-offset (beginning-of-line -1)
       )))))




(defun calp-connect ()
  (interactive)
  (geiser-connect-local 'guile (concat (getenv "XDG_RUNTIME_DIR")
                                       "/calp")))




;;; Other

(load-theme 'adwaita t t)

;; inhibit bidirectional parentheses algorith
;; might break rendering of RTL languages, but should speed up general rendering.
(setq bidi-inhibit-bpa t)

;; See M-x so-long-commentary
(when (version<= "27.1" emacs-version)
  (global-so-long-mode 1))

;;; Can I somehow enable this for all available modes?
(hook-envs #'hs-minor-mode
           '(emacs-lisp-mode-hook
             scheme-mode-hook
             lisp-mode-hook
             clojure-mode-hook
             c-mode-hook))

;; This should be bound to <RET> in shell-mode and term-mode,
;; but only when not in the active prompt area.
;;
;; It would be amazing if it was possible to let all file-names
;; store their path context, allowing any file to be opened with
;; this.
(defun open-file-at-point ()
  "Opens file under cursor in a new buffer.
Note that the user needs to stand in the same directory as the
file for it to work as expceted."
  (interactive)
  (find-file
   (thing-at-point 'filename)))

(defun describe-file-at-point ()
  "Run `file` on filename at point."
  (interactive)
  (popup-tip
   (shell-command-to-string
    (concat "file "
            (thing-at-point 'filename)))))

;;; This is /tmp/ by default
(setq temporary-file-directory
      (concat (or (getenv "XDG_CACHE_HOME")
                  (concat (getenv "HOME") "/.cache"))
              "/emacs"))

(mkdir temporary-file-directory t)

;;; Stores all temp files in one central location
(setq backup-directory-alist
        `((".*" . ,temporary-file-directory)))

(setq custom-file "~/.emacs.d/custom.el")
(load custom-file 'noerror)

(message ".emacs loaded in %ds"
         (let ((ct (current-time)))
          (- (+ (first ct) (second ct))
             (+ (first *emacs-load-start*)
                (second *emacs-load-start*)))))

(provide 'init)
;;; init.el ends here
