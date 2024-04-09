;;; init.el --- Init file for emacs
;;; Commentary:
;;; Code:

;; https://a-nickels-worth.blogspot.com/2007/11/effective-emacs.html
(defvar *emacs-load-start*
  (if (boundp 'time-convert)
    (time-convert (current-time) t)
    (current-time)))

(require 'package)

(setq package-archives
      '(("gnu" . "https://elpa.gnu.org/packages/")
        ("melpa" . "https://melpa.org/packages/")
        ("org" . "https://orgmode.org/elpa/")
        ))

;;; M-x package-install-selected-packages to actually install
(setq package-selected-packages
      `(
        ivy                      ; M-x fuzzy finder
        paredit
        popup
        which-key                ; show possible keys

        smart-tabs-mode

        evil evil-paredit
        ,@ (when (version< emacs-version "28")
             '(undo-fu))

        flycheck
        haskell-mode
        elm-mode
        markdown-mode
        cider
        slime

        magit
        git-gutter
        mmm-mode           ; Multiple Major Modes

        yasnippet
        yasnippet-snippets

        frames-only-mode

        evil-org

        geiser

        ;; geiser-chez
        ;; geiser-chibi
        geiser-chicken
        ;; geiser-gambit
        ;; geiser-gauche
        geiser-guile
        ;; geiser-kawa
        ;; geiser-mit
        geiser-racket
        ;; geiser-stklos

        ;; TODO This errors out in install-all,
        ;; but installing it manually works fine
        ;; irfc-mode

        ;; rainbow-delimiters
        ;; org-plus-contrib

        js2-mode
        tide

        lsp-mode
        svelte-mode
;; pyls pylsp ruff-lsp semgrep-ls

        ))

(package-initialize)

(package-refresh-contents t)
(package-install-selected-packages)



;;; Local Packages

(add-to-list 'load-path "~/.emacs.d/pkg")

;; Update local autoloads
(require 'autoload)
(setq generated-autoload-file "~/.emacs.d/pkg/my-autoloads.el")
;; Generate autoload file if none exist.
;; Note that this doesn't update it, so that still has to be done manually
(unless (file-exists-p generated-autoload-file)
  (update-directory-autoloads "~/.emacs.d/pkg"))
(require 'my-autoloads)



;;; Defines for making it this file better

(defmacro hook-envs (function environments)
  "Add function to list of hooks.
FUNCTION: function to run for all the hooks
ENVIRONMENTS: all hooks to bind to"
  `(mapc (lambda (hook)
           (add-hook hook ,function))
         ,environments))



(setq auto-save-file-name-transforms
      '((".*" "~/.cache/emacs/saves/" t)))



(require 'popup)


;;; Evil

;; must be set BEFORE (require 'evil)
(setq-default
 evil-undo-system
 (if (version<= "28" emacs-version)
     'undo-redo 'undo-fu))

(evil-mode)

(require 'evil-maps)                    ; Is this required?

;; (key "J") runs join-line, which uses the fill prefix,
;; which is set by (key "C-x .") (`set-fill-line`).
;; TODO do this automatically somehow?

(setq evil-split-window-below t
      evil-vsplit-window-right t
      )

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
(add-hook 'geiser-repl-mode-hook 'def-read-tag-name)
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
  (kbd "C-u") 'evil-scroll-up
  (kbd "C-p") 'project-find-files)

(with-eval-after-load 'paredit
  (evil-define-key '(motion) paredit-mode-map
    (kbd "C-k") 'kill-sexp))

;;; [RET] is the "primitive" return code, exactly the same as ^M
;;; <return> is a high level construct, and only available in GUI.
(define-key evil-normal-state-map (kbd "RET") 'evil-fresh-line-below)
(define-key evil-normal-state-map (kbd "<S-return>") 'evil-fresh-line-above)

;; (evil-define-key '(normal insert) evil-org-mode-map
;;   (kbd "RET") 'evil-org-open-below)

(evil-define-key '(normal) evil-org-mode-map
  (kbd "g l l") 'org-indent-item)

(evil-define-key '(normal) evil-org-mode-map
  (kbd "g l h") 'org-outdent-item)



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
            ;; TODO this overrides the global bindings
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



(with-eval-after-load 'org-mode
  (evil-define-key 'normal org-mode-map (kbd "z j")
    'org-forward-heading-same-level)
  (evil-define-key 'normal org-mode-map (kbd "z k")
    'org-backward-heading-same-level)
  (setq-default
   org-treat-insert-todo-heading-as-state-change t
   org-hide-leading-stars t
   org-agenda-default-appointment-duration 60
   org-todo-keyword-faces '(("TEST" . "orange"))
   ))

(add-hook 'org-mode-hook #'evil-org-mode)


;;; Prettify

;;; Setq's updates prettify-symbols-alist localy for that buffer in that mode
;;; (or something like that), which is exactly what we want, but means that we
;;; need to set it in a hook (instead of with an with-eval-after-load).

(defun prettify-scheme ()
  (setq prettify-symbols-alist
        (append
         prettify-symbols-alist
         '(("lambda" . #x3bb)           ; λ
           ("<=" . #x2264)              ; ≤
           (">=" . #x2265)              ; ≥
           ("memv" . ?∈)
           ;; ("sum" . #x2211)              ; ∑
           ;; ("prod" . #x220f)             ; ∏
           ;; These are nice, but my font doesn't render them
           ;; correctly monospaced
           ;; ("'()" . ?∅)
           ;; ("or" . ?⋁)
           ;; ("and" . ?⋀)
           ))))

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

(dolist (p '("/home/hugo/.local/share/info" "/usr/local/share/info"))
  (add-to-list 'Info-default-directory-list p))

(with-eval-after-load 'info
  (evil-define-key 'motion Info-mode-map
    "l" 'Info-last
    "N" 'evil-search-next
    "P" 'evil-search-previous
    "[" 'Info-backward-node
    "]" 'Info-forward-node
    "n" 'Info-next
    "p" 'Info-prev
    (kbd "<tab>") 'Info-next-reference
    "f" 'Info-follow-reference
    "g" 'Info-goto-node
    (kbd "RET") 'Info-follow-nearest-node
    ))

(evil-define-key 'motion help-mode-map
  (kbd "<tab>") 'forward-button)



;; Emacs mode works better in the shell,
;; But keep the window commands every other window uses.
(with-eval-after-load 'esh-mode
  (evil-emacs-state)
  (keymap-set eshell-mode-map "C-w h" #'evil-window-left)
  (keymap-set eshell-mode-map "C-w j" #'evil-window-down)
  (keymap-set eshell-mode-map "C-w k" #'evil-window-up)
  (keymap-set eshell-mode-map "C-w l" #'evil-window-right)
  (setq-local show-trailing-whitespace nil))





(add-to-list 'auto-mode-alist '("rfc[0-9]+\\.txt\\'" . irfc-mode))

(with-eval-after-load 'irfc
 (evil-define-key 'normal irfc-mode-map
   "t" 'irfc-head-goto
   "[[" 'irfc-head-prev
   "]]" 'irfc-head-next
   "]x" 'irfc-page-next
   "[x" 'irfc-page-prev
   "j" 'scroll-up-line
   "k" 'scroll-down-line
   (kbd "RET") 'irfc-follow))

(add-hook 'irfc-mode-hook #'flyspell-mode-off)

(setq irfc-directory "~/.local/doc/rfc/")
(defvar rfc-index-file (concat irfc-directory "rfc-index.txt"))

;; TODO download rfc-index.txt from
;; https://www.ietf.org/download/rfc-index.txt

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

(with-eval-after-load 'paredit
  (define-key paredit-mode-map (kbd "C-j")
    'eval-sexp-print)

  (define-key paredit-mode-map (kbd "C-S-j")
    'eval-sexp)

  (add-hook 'paredit-mode-hook #'evil-paredit-mode))

(hook-envs #'enable-paredit-mode
           '(emacs-lisp-mode-hook
             eval-expression-minibuffer-setup-hook
             ielm-mode-hook
             lisp-mode-hook
             lisp-interaction-mode-hook
             lisp-data-mode-hook
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

  ;; TODO With eval after load?
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
    (if (equal "=> " ret)
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
    (popup-tip (if (equal ret "⇒ ")
                   "EVALUATION ERROR"
                 ret))))

(with-eval-after-load 'scheme
   (require 'geiser)

   ;; Indents test-group similar to defines
   (put 'test-group              'scheme-indent-function 1)
   (put 'test-group-with-cleanup 'scheme-indent-function 1)
   (put 'test-eqv                'scheme-indent-function 1)
   (put 'test-eq                 'scheme-indent-function 1)
   (put 'test-equal              'scheme-indent-function 1)
   (put 'test-approximate        'scheme-indent-function 1)
   (put 'test-error              'scheme-indent-function 1)
   (put 'test-assert             'scheme-indent-function 1)
   (put 'with-atomic-output-to-file 'scheme-indent-function
        (get 'with-output-to-file 'scheme-indent-function))

   (font-lock-add-keywords
    'scheme-mode
    `(,(regexp-opt '("mod!" "set!") 'symbols)
      ;; suffix keywords (srfi-71)
      ("\\<\\w+:\\>" . font-lock-constant-face)
      ;; Character literals
      ("#\\\\\\(.\\w*\\)" . (1 font-lock-string-face))
      ;; Everithyng starting with `define-' is a form of define
      ("(\\<\\(define-\\w*\\)\\>\s +(?\\(\\S +\\)?"
       (1 ,font-lock-keyword-face) (2 ,font-lock-function-name-face))
      ("\\<with-\\w*\\>" . font-lock-keyword-face)
      )))


(require 'mmm-auto)

(with-eval-after-load 'mmm-mode
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
     )
    (scheme-bash-shebang
     :submode shell-script-mode
     :front "^#!"
     :back "^!#"
     :include-front t
     :include-back nil)))

 (mmm-add-mode-ext-class 'scheme-mode nil 'lisp-texinfo-comments)
 (mmm-add-mode-ext-class 'scheme-mode nil 'scheme-bash-shebang)
 )

(add-hook
 'scheme-mode-hook
 (lambda ()
   (geiser-mode)
   ;;; mmm-mode apparently requires scheme-version to be set
   ;; but it does ask if it doesn't know
   (mmm-mode)

   ;; Let's pretend any scheme buffer is an interaction scheme buffer!
   ;; geiser-eval-last-sexp doesn't like guile reader extensions ("#")
   (setq *eval-sexp-print* 'geiser-eval-print-last-sexp
         *eval-sexp*       'geiser-eval-popup-last-sexp)
   (setq-local evil-lookup-func  #'geiser-doc-symbol-at-point)

   ))

;; geiser-repl-mode

(with-eval-after-load 'geiser
  (defvar os-id (shell-command-to-string
                 "awk -F= -v ORS='' '/^ID=/ { print $2 }' /etc/os-release"))

  (setq-default
   ;; Geiser only looks at these, if this list is here
   geiser-active-implementations '(guile chicken racket)
   geiser-default-implementation 'guile

   geiser-repl-per-project-p t

   geiser-guile-init-file "~/.config/geiser/guile.scm"

   geiser-chicken-binary (pcase os-id
                           ("arch" "chicken-csi")
                           ("ubuntu" "csi")
                           (_ "csi")
                           )
   )

  (evil-define-key '(normal emacs insert) geiser-repl-mode-map
    (kbd "C-l") 'geiser-repl-clear-buffer
    (kbd "M-.") 'geiser-edit-symbol-at-point
    (kbd "C-]") 'geiser-edit-symbol-at-point)

  (evil-define-key '(normal) geiser-debug-mode-map
    (kbd ",") 'geiser-debug--debugger-transient)

  (evil-define-key '(normal insert) scheme-mode-map
    (kbd "M-.") 'geiser-edit-symbol-at-point
    (kbd "C-]") 'geiser-edit-symbol-at-point)

  ;; geiser-guile-extra-keywords
  ;; geiser-guile-init-file
  ;; geiser-guile-load-init-file-p


  ;; TODO
  ;; /usr/share/emacs/26.3/lisp/progmodes/etags.el.gz
  ;; Sätt upp det här som en xref-backend
  ;; (evil-ex-define-cmd "ta[g]" 'geiser-edit-symbol)
  )

(add-to-list 'auto-mode-alist '("\\.log\\'" . srfi-64-log-mode))





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



;; Python

(with-eval-after-load 'python
  (evil-define-key '(visual) python-mode-map
    (kbd "C-j") 'python-shell-send-region))



(defun setup-cmake-mode ()
  (require 'cmake-mode)
  (cmake-mode))

(add-to-list 'auto-mode-alist '("CMakeLists\\.txt\\'" . setup-cmake-mode))



(defun calp-connect ()
  (interactive)
  (geiser-connect-local 'guile (concat (getenv "XDG_RUNTIME_DIR")
                                       "/calp")))



;; Copied from https://github.com/ananthakumaran/tide
(defun setup-tide-mode ()
  (interactive)
  (tide-setup)
  (flycheck-mode +1)
  (setq flycheck-check-syntax-automatically '(save mode-enabled))
  (eldoc-mode +1)
  (tide-hl-identifier-mode +1)
  ;; company is an optional dependency. You have to
  ;; install it separately via package-install
  ;; `M-x package-install [ret] company`
  ; (company-mode +1)
  )

;; aligns annotation to the right hand side
(setq company-tooltip-align-annotations t)

;; formats the buffer before saving
(add-hook 'before-save-hook 'tide-format-before-save)

(add-hook 'typescript-mode-hook #'setup-tide-mode)



;;; Other

(setq browse-url-handlers
      '(("^https?://www.lispworks.com/documentation/HyperSpec/.*" . eww)))

(add-hook 'Buffer-menu-mode-hook #'hl-line-mode)
(evil-define-key '(motion) Buffer-menu-mode-map
  (kbd "C-d") 'evil-scroll-down
  (kbd "$") (lambda () (interactive)
              (when (y-or-n-p "Kill selected buffers?")
                       (Buffer-menu-execute))))


;;; Dired

(with-eval-after-load 'dired
  (add-hook 'dired-mode-hook #'hl-line-mode)

  (evil-define-key '(normal) dired-mode-map
    (kbd "G") 'evil-goto-line
    (kbd "g g") 'evil-goto-first-line
    ;; delete all entries flagged for deletion
    ;; prompts beforehand
    (kbd "$") 'dired-do-flagged-delete
    ))


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

(evil-define-key '(normal) 'global
  (kbd "C-p") 'project-find-file)

;; https://stackoverflow.com/questions/6083496/how-do-you-specify-a-fallback-font-in-emacs
;; pacman -S bdf-unifont
(set-fontset-font t nil (font-spec :size 20 :name "Unifont"))


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
         (if (boundp 'time-convert)
           (let ((ct (time-convert (current-time) t)))
             (/ (- (car ct) (car *emacs-load-start*))
                (* 1.0 (cdr ct))))
           (let ((ct (current-time)))
             (- (+ (car ct) (cadr ct))
                (+ (car *emacs-load-start*)
                   (cadr *emacs-load-start*))))))

(provide 'init)
;;; init.el ends here
(put 'narrow-to-page 'disabled nil)
