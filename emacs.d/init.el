(require 'package)
(require 'cl)

(setq package-archives
      '(("gnu" . "https://elpa.gnu.org/packages/")
        ("melpa" . "https://melpa.org/packages/")))

;;; ==Speedbar==
;;; z should toggle expand|contract
;;; speedbar-expand-line
;;; speedbar-contract-line

;;; TODO
;;; extend theme to show comments and comment markers in different colors. 

(setq required-packages
      `(flycheck
        haskell-mode
        ivy                             ; M-x fuzzy finder
        ;; magit
        mmm-mode                        ; Multiple Major Modes
        paredit
        popup
        smart-tabs-mode
        which-key                       ; show possible keys
        ))

(setq evil-packages `(evil evil-org evil-paredit))

(setq all-packages `(,@ required-packages ,@ evil-packages))

(package-initialize)

(setq packages-to-install
      (seq-remove #'package-installed-p
                  all-packages))

(when packages-to-install
  (package-refresh-contents)
  (mapc #'package-install packages-to-install))

(mapc #'require required-packages)

(defun safe-load-pkg (pkg)
  (unless (package-installed-p pkg)
    (package-install pkg))  )

;;; -------- Local Packages ------------------------------------

(add-to-list 'load-path "~/.emacs.d/pkg")
(add-to-list 'load-path "/usr/local/share/emacs/site-lisp")

;; (load-library "noweb-mode.el")
(autoload 'noweb-mode "noweb-mode" "Editing noweb files." t)
(setq auto-mode-alist (append (list (cons "\\.nw$" 'noweb-mode))
			      auto-mode-alist))

(autoload 'lyskom "lyskom.elc" "LysKOM" t)

;;; --------- Defines for making it this file better -----------


(defmacro hook-envs (function environments)
  "Add function to list of hooks"
  `(mapc (lambda (hook)
           (add-hook hook ,function))
         ,environments))

;;; This macro expands correctly, but the require doesn't work
;;; from inside a macro for some reason.
(defmacro evil-collection-load (mode)
  (let ((sname  (symbol-name mode)))
    `(with-eval-after-load (quote ,mode)
       (require (quote ,(intern (concat "evil-collection-" sname))))
       (,(intern (concat "evil-collection-" sname "-setup"))))))


;;; ----------- Evil -------------------------------------------

(mapc #'require evil-packages)

(evil-mode)

(require 'evil-maps)                    ; Is this required?

(evil-ex-define-argument-type tag
  "Handles tag names"
  ;; This is a function that should take stuff...
  ;; :collection complete-tag
  )

(evil-define-interactive-code "<tag>"
  :ex-arg tag
  (list (when (evil-ex-p) evil-ex-argument)))

(evil-define-command vi-follow-tag (tag &rest rest)
  :repeat nil
  (interactive "<tag>")
  (princ tagname)
  (princ rest)
  (tags-search tag))

;; eval-expression

; C-<tab> does tag completion

(evil-ex-define-cmd "vsp" "vsplit")
;;; TODO vsb, vertical split buffer
(evil-ex-define-cmd "ta[g]" 'vi-follow-tag)

;;; ------------------------------------------------------------
(load-theme 'wombat)

(ivy-mode)
(which-key-mode) ; Show possible next keys after some key presses 
(show-paren-mode)
(column-number-mode)

(menu-bar-mode 0)
(tool-bar-mode 0)
(scroll-bar-mode -1)

(setq inhibit-startup-screen t)

(define-key evil-normal-state-map "\C-u" 'evil-scroll-up)
(define-key evil-motion-state-map "\C-u" 'evil-scroll-up)
;; TODO This should only be for paredit-mode
(define-key evil-motion-state-map "\C-k" 'kill-sexp)
;;; <CR> should be bound to (normal "o<esc>")
;; (define-key evil-normal-state-map (string ?\n) 'evil-open-below)

;;; -------- Whitespace ----------------------------------------

(setq-default indent-tabs-mode nil)

(smart-tabs-insinuate
 'c 'c++ 'java 'javascript
 'python 'ruby 'cperl 'ruby)

;;; this should make evil-mode work with smart-tabs-mode,
;;; Mostly for `<' & `>' shifting. But it doesn't.
(setq evil-indent-convert-tabs nil)

;;; -------- Org Mode ------------------------------------------

(defun org-mode-stuff ()
  (evil-define-key 'normal org-mode-map (kbd "z j")
    'org-forward-heading-same-level)
  (evil-define-key 'normal org-mode-map (kbd "z k")
    'org-backward-heading-same-level))

(add-hook 'org-mode-hook #'evil-org-mode)
(add-hook 'org-mode-hook #'org-mode-stuff)

;;; ------- Prettify -------------------------------------------

(defun prettify-scheme ()
  (setq prettify-symbols-alist
        '(("lambda" . #x3bb)
          ("<=" . #x2264)
          (">=" . #x2265)
          ("memv" . ?∈)
          ("sum" . #x2211)
          ("prod" . #x220f))))
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
           ))))

(add-hook 'tex-mode-hook #'prettify-tex)

(global-prettify-symbols-mode 1)

;;; --------- TexInfo ------------------------------------------

(loop for p in '("/home/hugo/info" "/usr/local/share/info")
      do (add-to-list 'Info-default-directory-list p))
(defun info-binds ()
  (evil-define-key 'motion Info-mode-map "l" 'Info-last)
  ;; Find non-conflicting binding for this
  ;; (evil-define-key 'motion Info-mode-map "n" 'evil-search-next)
  )
(add-hook 'Info-mode-hook #'info-binds)


;;; ----------- Paredit ----------------------------------------

(defun paredit-stuff ()
  (evil-define-key 'visual lisp-mode-map
    (kbd "SPC ;") 'paredit-comment-dwim)
  (enable-paredit-mode))

'(comment ; Something like this should be used insead,
  (use-modules (ice-9 pretty-print))
  (call-with-input-string
   (string-drop "=> (1 (2 (3 (a) b) c))" 3)
   (compose pretty-print
            read)))

(defun eval-sexp-print () (interactive)
       (princ "No eval-sexp-print for current mode."))
(defun eval-sexp () (interactive)
       (princ "No eval-sexp for current mode."))


(define-key paredit-mode-map (kbd "C-j")   #'eval-sexp-print)
(define-key paredit-mode-map (kbd "C-S-j") #'eval-sexp)

(add-hook 'paredit-mode-hook #'evil-paredit-mode)

(hook-envs
 #'paredit-stuff
 '(emacs-lisp-mode-hook
   eval-expression-minibuffer-setup-hook
   ielm-mode-hook
   lisp-mode-hook
   lisp-interaction-mode-hook
   scheme-mode-hook
   clojure-mode-hook))


;;; ---------- Emacs Lisp --------------------------------------

(defun elisp-eval-popup ()
  (interactive)
  (popup-tip
   (with-output-to-string
     (princ (eval (elisp--preceding-sexp))))))

(hook-envs
 (lambda ()
   (defalias 'eval-sexp-print #'eval-print-last-sexp)
   (defalias 'eval-sexp #'elisp-eval-popup))
 '(emacs-lisp-mode-hook
   lisp-interaction-mode-hook))

;;; ---------- Common Lisp -------------------------------------

(add-hook 'lisp-mode-hook
 (lambda () (safe-load-pkg 'slime)))

;;; ---------- Clojure -----------------------------------------

(defun clojure-env ()
  (safe-load-pkg 'cider)

  (define-clojure-indent
    (defroutes 'defun)
    (GET 2) (POST 2) (PUT 2)
    (DELETE 2) (HEAD 2) (ANY 2)
    (OPTIONS 2) (PATCH 2) (rfn 2)
    (let-routes 1) (context 2)
    (html5 2)
    (full-page 1)))

(add-hook 'clojure-mode-hook #'clojure-env)

;;; -----------Scheme / Geiser ---------------------------------

;;; Geiser seems to not work to well with evaling sexp's directly. I should
;;; however be able to create a new buffer, and eval the above there.
          
;; =C-u C-u M-x geiser-eval-last-sexp= does this,
;; But without the fancy formatting!
(defun geiser-eval-print-last-sexp ()
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
  (interactive)
  (let ((ret (geiser-eval-last-sexp nil)))
    (popup-tip (if (equalp ret "=> ")
                   "EVALUATION ERROR"
                 ret))))


;; Let's pretend any scheme buffer is an interaction scheme buffer!
;; geiser-eval-last-sexp doesn't like guile reader extensions ("#")
(add-hook
 'scheme-mode-hook
 (lambda ()
   (safe-load-pkg 'geiser)
   (defalias 'eval-sexp-print #'geiser-eval-print-last-sexp)
   (defalias 'eval-sexp #'geiser-eval-last-sexp)
   (font-lock-add-keywords
    nil `(,(regexp-opt '("mod!" "set!") 'symbols)
          ("\\<\\(define-\\w*\\)\\>\s*(?\\(\\sw+\\)?"
           (1 ,font-lock-keyword-face) (2 ,font-lock-function-name-face))))))

;;; TODO add optional path argument, which should be able to be given through M-x
(defun gamesh-connect ()
  (interactive)
  (geiser-connect-local 'guile "/tmp/guile-gamesh-repl"))

;; geiser-repl-mode

;; Geiser only looks at these, if this list is here 
(setq geiser-active-implementations '(guile racket))

;;; ----------- Haskell ----------------------------------------

(add-hook 'haskell-mode-hook 'my-mmm-mode)

(mmm-add-classes
 '((literate-haskell-bird
    :submode text-mode
    :front "^[^>]"
    :include-front true
    :back "^>\\|$"
    )
   (literate-haskell-latex
    :submode literate-haskell-mode
    :front "^\\\\begin{code}"
    :front-offset (end-of-line 1)
    :back "^\\\\end{code}"
    :include-back nil
    :back-offset (beginning-of-line -1)
    )))

(defun my-mmm-mode ()
  ;; go into mmm minor mode when class is given
  (make-local-variable 'mmm-global-mode)
  (setq mmm-global-mode 'true))

(setq mmm-submode-decoration-level 0)

;;; ------------------------------------------------------------


;;; ---------- Other -------------------------------------------

;;; Can I somehow enable this for all available modes?
(hook-envs #'hs-minor-mode
           '(emacs-lisp-mode-hook
             scheme-mode-hook
             lisp-mode-hook
             clojure-mode-hook
             c-mode-hook))


;;; geiser should also log commands which failed
;;; I believe that it currently only logs those
;;; which exited successfully
(setq geiser-repl-history-filename
      "~/.emacs.d/geiser/history")

;; This should be bound to <RET> in shell-mode and term-mode,
;; but only when not in the active prompt area.
;;
;; It would be amazing if it was possible to let all file-names
;; store their path context, allowing any file to be opened with
;; this.
(defun open-file-at-point ()
  "Opens file under cursor in a new buffer
Note that the user needs to stand in the same directory as the
file for it to work as expceted."
  (interactive)
  (find-file
   (thing-at-point 'filename)))

(defun describe-file-at-point ()
  (interactive)
  (popup-tip
   (shell-command-to-string
    (concat "file "
            (thing-at-point 'filename)))))

;;; This is /tmp/ by default
;;; TODO
;;; This currently brakes if ~/.cache/emacs doesn't
;;; exists, do something about that.
(setq temporary-file-directory
      (or (getenv "XDG_CACHE_HOME")
          (concat (getenv "HOME")
                  "/.cache/emacs")) )

;;; Stores all temp files in one central locatio n
(setq backup-directory-alist
        `((".*" . ,temporary-file-directory)))

(setq org-treat-insert-todo-heading-as-state-change t 
      org-hide-leading-stars t
      org-agenda-default-appointment-duration 60)

(setq-default fill-column 80)
(add-hook 'tex-mode-hook (lambda () (setq fill-column 60)))

(setq custom-file "~/.emacs.d/custom.el")
(load custom-file 'noerror)
