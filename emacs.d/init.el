;;; init.el --- Init file for emacs
;;; Commentary:
;;; Code:

(eval-when-compile (require 'cl))
(require 'package)

(setq package-archives
      '(("gnu" . "https://elpa.gnu.org/packages/")
        ("melpa" . "https://melpa.org/packages/")))

(defvar required-packages)
(setq required-packages
      `(flycheck
        haskell-mode
        ivy                             ; M-x fuzzy finder
        magit
        mmm-mode                        ; Multiple Major Modes
        paredit
        popup
        smart-tabs-mode
        which-key                       ; show possible keys
        yasnippet
        yasnippet-snippets
        ;; markdown-mode
        frames-only-mode
        ;; rainbow-delimiters
        elm-mode
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
    (package-install pkg)))


;;; Local Packages

(add-to-list 'load-path "~/.emacs.d/pkg")
(add-to-list 'load-path "/usr/local/share/emacs/site-lisp")

;; (load-library "noweb-mode.el")
(autoload 'noweb-mode "noweb-mode" "Editing noweb files." t)
(setq auto-mode-alist (append (list (cons "\\.nw$" 'noweb-mode))
                              auto-mode-alist))

(autoload 'lyskom "lyskom.elc" "LysKOM" t)


;;; Defines for making it this file better

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

(defun re-seq (regexp string)
  (save-match-data
    (let ((pos 0)
          matches)
      (while (string-match regexp string pos)
        (push (match-string 0 string) matches)
        (setq pos (match-end 0)))
      (reverse matches))))


;;; Evil

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

(defun evil-fresh-line-below (&optional count)
  "open-below, followed by returning to normal mode."
  (interactive "p")
  (evil-open-below count)
  (evil-normal-state))

(defun evil-fresh-line-above (&optional count)
  "open-above, followed by returning to normal mode."
  (interactive "p")
  (evil-open-above count)
  (evil-normal-state))

(evil-define-key '(normal motion) 'global
  (kbd "C-u") 'evil-scroll-up)

(evil-define-key '(motion) paredit-mode-map
  (kbd "C-k") 'kill-sexp)

;;; [RET] is the "primitive" return code, exactly the same as ^M
;;; <return> is a high level construct, and only available in GUI.
(define-key evil-normal-state-map (kbd "RET") 'evil-fresh-line-below)
(define-key evil-normal-state-map (kbd "<S-return>") 'evil-fresh-line-above)




(load-theme 'wombat)

(ivy-mode)
(which-key-mode) ; Show possible next keys after some key presses
(show-paren-mode)
(column-number-mode)

(frames-only-mode)

(menu-bar-mode 0)
(tool-bar-mode 0)
(scroll-bar-mode -1)

;; (defvar og-whitespace-style whitespace-style)
;; Highlight "bad" whitespace.
;; Unfortunately breaks "regular" whitespace mode.
;; (setq whitespace-style
;;       '(face space-before-tab trailing))

;; Mark lines not part of file.
(setq-default indicate-empty-lines t)
(setq-default show-trailing-whitespace t)

(add-hook 'read-only-mode-hook
          #'(lambda () (message "Hello")
              (setq-local show-trailing-whitespace (not buffer-read-only))))


(setq inhibit-startup-screen t)

(setq mmm-submode-decoration-level 0)
(setq mmm-global-mode 'maybe)


;;; Formfeed

(defun insert-formfeed ()
  (interactive)

  (unless (= (point) (point-at-eol))
    (setf (point) (point-at-eol)))

  (unless (= (point-at-bol) (point-at-eol))
    (newline))

  (insert-char ?\^L)
  (newline))

(defun xah-show-formfeed-as-line ()
  "Display the formfeed ^L char as line.
URL `http://ergoemacs.org/emacs/emacs_form_feed_section_paging.html'
Version 2018-08-30"
  ;; (interactive)
  ;; 2016-10-11 thanks to Steve Purcell's page-break-lines.el
  (progn
    (when (not buffer-display-table)
      (setq buffer-display-table (make-display-table)))
    (aset buffer-display-table ?\^L
          (make-vector
           (round (* (window-width)
                     (if (and (boundp 'text-scale-mode)
                              (symbol-value 'text-scale-mode))
                         ;; TODO this gives rather bad values
                         (expt text-scale-mode-step (- text-scale-mode-amount))
                       1)))
           (make-glyph-code ?─          ; 'font-lock-comment-face
                            )))
    (redraw-frame)))

(define-minor-mode formfeed-mode
  "Mode for handling formfeeds as content changes."
  :lighter " ^L"
  :global t
  :keymap (make-sparse-keymap)
  ;; :after-hook (redraw-frame)
  (defvar *^L* nil "Previous value of buffer-display-table[^L]")
  (progn
    (when (not buffer-display-table)
      (setq buffer-display-table (make-display-table)))

    ;; TODO window-size-change-functions seem to only be triggered in the
    ;; currently selected window
    (if formfeed-mode
        (progn
          (setq *^L* (aref buffer-display-table ?\^L))
          (add-to-list ; NOTE this is per window, making this mode hard to turn off.
           'window-size-change-functions
           (lambda (frame) (xah-show-formfeed-as-line)))

          ;; These are never turned off, fore some reason
          (evil-define-minor-mode-key '(normal visual) formfeed-mode
            "[x" 'backward-page
            "]x" 'forward-page)
          (evil-define-minor-mode-key '(normal insert replace) formfeed-mode
            (kbd "C--") 'insert-formfeed)

          (xah-show-formfeed-as-line))

      (progn
        (aset buffer-display-table ?\^L *^L*)
        (setq window-size-change-functions ())))))


;; TODO this is't imidieately run for new buffers, instead first updating when
;; the window first changes shape.
(formfeed-mode 1)


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
    'org-backward-heading-same-level))

(add-hook 'org-mode-hook #'evil-org-mode)
(add-hook 'org-mode-hook #'org-mode-stuff)


;;; Prettify

(defun prettify-scheme ()
  (setq prettify-symbols-alist
        '(("lambda" . #x3bb)            ; λ
          ("<=" . #x2264)               ; ≤
          (">=" . #x2265)               ; ≥
          ("memv" . ?∈)
          ("sum" . #x2211)              ; ∑
          ("prod" . #x220f))))          ; ∏
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


;; (autoload 'irfc "irfc-visit")
;; https://www.emacswiki.org/emacs/Irfc
(unless (file-exists-p "~/.emacs.d/pkg/irfc.el")
  (url-copy-file "https://www.emacswiki.org/emacs/download/irfc.el"
                 "~/.emacs.d/pkg/irfc.el" nil))
(require 'irfc)

(evil-define-key 'normal irfc-mode-map
  "t" 'irfc-head-goto
  "[[" 'irfc-head-prev
  "]]" 'irfc-head-next
  "]x" 'irfc-page-next
  "[x" 'irfc-page-prev)

;; (irfc-reference-goto)

(setq irfc-directory "~/.local/share/RFC/")
(defvar rfc-index-file (concat irfc-directory "rfc-index.txt"))
(defun rfc-list ()
  "Parees `rfc-index-file` for all RFC's.
TODO I should filter out obsoleted matches"
  (re-seq
   "^[0-9]+ .*"
   (with-current-buffer (find-file-noselect rfc-index-file)
     (buffer-string))))

(defun rfc-goto (str &rest rest)
  "irfc-visit, with a list of all RFC's"
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

(defun eval-sexp-print () (interactive)
       (funcall (symbol-function *eval-sexp-print*)))
(defun eval-sexp () (interactive)
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
   (safe-load-pkg 'slime)
   (setq *eval-sexp-print* 'slime-eval-print-last-expression
         *eval-sexp*       'slime-eval-last-expression)))


;;; Clojure

;; tex-latex-indent-syntax-table

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


;;; Scheme / Geiser

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
    (popup-tip (if (equalp ret "⇒ ")
                   "EVALUATION ERROR"
                 ret))))


;; Let's pretend any scheme buffer is an interaction scheme buffer!
;; geiser-eval-last-sexp doesn't like guile reader extensions ("#")
(add-hook
 'scheme-mode-hook
 (lambda ()
   (safe-load-pkg 'geiser)
   (geiser-mode)
   (setq *eval-sexp-print* 'geiser-eval-print-last-sexp
         *eval-sexp*       'geiser-eval-popup-last-sexp)
   (font-lock-add-keywords
    nil `(,(regexp-opt '("mod!" "set!") 'symbols)
          ("\\<\\w+:\\>" . font-lock-constant-face)
          ("(\\<\\(define-\\w*\\)\\>\s +(?\\(\\S +\\)?"
           (1 ,font-lock-keyword-face) (2 ,font-lock-function-name-face))))))

;;; TODO add optional path argument, which should be able to be given through M-x
(defun gamesh-connect ()
  (interactive)
  (geiser-connect-local 'guile "/tmp/guile-gamesh-repl"))

(evil-define-key '(normal emacs insert) geiser-repl-mode-map
  (kbd "C-l") 'geiser-repl-clear-buffer)

;; geiser-repl-mode

;; Geiser only looks at these, if this list is here
(setq geiser-active-implementations '(guile chicken))
(setq geiser-chicken-binary "chicken-csi")
(when (string= "lysator.liu.se" (string-trim-right (shell-command-to-string "hostname -d")))
  (setq geiser-chicken-binary "csi"))

(setq geiser-guile-load-path '("/home/hugo/lib/guile" "."))
;; geiser-guile-extra-keywords
;; geiser-guile-init-file
;; geiser-guile-load-init-file-p

(evil-define-key '(normal insert) scheme-mode-map
  (kbd "M-.") 'geiser-edit-symbol-at-point)

;; extend theme to show comments and comment markers in different colors.

(mmm-add-classes
 '((lisp-texinfo-comments
    :submode texinfo-mode
    :front "^;; "
    ;; :back "^[^;]"
    :back "$"
    :include-front nil
    :include-back nil
    )))

(mmm-add-mode-ext-class 'scheme-mode nil 'lisp-texinfo-comments)


;;; Haskell

;; (add-hook 'haskell-mode-hook 'my-mmm-mode)

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



(yas-global-mode)



;;; Other

;; (defun complete ()
;;   (interactive)
;;   (let ((abbrev (dabbrev--abbrev-at-point)))
;;     (message abbrev)
;;     (setq dabbrev--last-abbreviation abbrev)
;;     (let ((lst (dabbrev--find-all-expansions abbrev t)))
;;       (if (not lst)
;;           (message "No completions at point")
;;         (popup-menu* lst)))))

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

;;; Stores all temp files in one central location
(setq backup-directory-alist
        `((".*" . ,temporary-file-directory)))

(setq org-treat-insert-todo-heading-as-state-change t
      org-hide-leading-stars t
      org-agenda-default-appointment-duration 60)

(setq-default fill-column 80)
(add-hook 'tex-mode-hook (lambda () (setq fill-column 60)))

(setq custom-file "~/.emacs.d/custom.el")
(load custom-file 'noerror)

;; (add-hook 'before-save-hook 'delete-trailing-whitespace)

(global-set-key (kbd "M-p") 'other-window)
