(require 'package)

(setq package-archives
      '(("gnu" . "https://elpa.gnu.org/packages/")
        ("melpa" . "https://melpa.org/packages/")))

;;; TODO
;;; ==Ivy==
;;; - show bindings when M-x
;;; ==Evil==
;;; :vsp <file>
;;; :vsplit <file> works, but the short version doesn't
;;; ==Speedbar==
;;; z should toggle expand|contract
;;; speedbar-expand-line
;;; speedbar-contract-line

(setq required-packages
      `(
        calfw
        calfw-org
        geiser
        ivy ; fuzzy finder
        magit
        ;; org-expiry
        paredit
        popup
        smart-tabs-mode
        which-key ; show possible keys

        ;; When X-window
        ,@ (when (display-graphic-p)
             '(xresources-theme))
        ))

(setq evil-packages
      `(evil
        ;; evil-collection
        evil-magit
        evil-org
        ;; evil-org-agenda
        evil-paredit
        ))

(setq all-packages
      `(
        ,@ required-packages
        ,@ evil-packages
        ))

(package-initialize)

(setq packages-to-install
      (seq-remove #'package-installed-p
                  all-packages))

(when packages-to-install
  (package-refresh-contents)
  (mapc #'package-install packages-to-install))

(mapc #'require required-packages)

;;; ------------------------------------------------------------

(add-to-list 'load-path "~/.emacs.d/pkg")
(add-to-list 'load-path "/usr/local/share/emacs/site-lisp")

(autoload 'lyskom "lyskom.elc" "LysKOM" t)

;;; ------------------------------------------------------------

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


;;; ------------------------------------------------------------

;;; TODO: Evil collection wants this, check why
;;; It's currently disabled because it makes show-paren-mode
;;; have a off by one error
;; (setq evil-want-integration nil)

(mapc #'require evil-packages)

(evil-mode)

;;; I think this inits everything at once
;; (evil-collection-init)

;; (evil-collection-load calendar)

;;; TODO: The simple reqire works, but the macro does't
;; (require 'evil-collection-eww)
;; (evil-collection-load eww)

(with-eval-after-load 'geiser
  (require 'evil-collection-geiser)
  (evil-collection-geiser-setup))

(with-eval-after-load 'eww
  (require 'evil-collection-eww)
  (evil-collection-eww-setup))

;;; ------------------------------------------------------------


(ivy-mode)
(which-key-mode) ; Show possible next keys after some key presses 
(show-paren-mode)
(column-number-mode)

(menu-bar-mode 0)
(tool-bar-mode 0)
(scroll-bar-mode -1)
(text-scale-set -1)

(setq inhibit-startup-screen t)

(define-key evil-normal-state-map "\C-u" 'evil-scroll-up)
(define-key evil-motion-state-map "\C-u" 'evil-scroll-up)
;;; <CR> should be bound to (normal "o<esc>")
;; (define-key evil-normal-state-map (string ?\n) 'evil-open-below)

(setq-default indent-tabs-mode nil)

(smart-tabs-insinuate
 'c 'c++ 'java 'javascript
 'python 'ruby 'cperl 'ruby)

;;; this should make evil-mode work with smart-tabs-mode,
;;; Mostly for `<' & `>' shifting. But it doesn't.
(setq evil-indent-convert-tabs nil)

(add-hook 'org-mode-hook #'evil-org-mode)

(defun prettify-scheme ()
  (setq prettify-symbols-alist
        '(("lambda" . #x3bb)
          ("<=" . #x2264)
          (">=" . #x2265)
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
           ("\\sqrt" . ?√)
           ("\\left(" . ?\()
           ("\\right)" . ?\))))))
(add-hook 'tex-mode-hook #'prettify-tex)

(global-prettify-symbols-mode 1)

(add-to-list 'Info-default-directory-list "/home/hugo/info")
(defun info-binds ()
  (evil-define-key 'motion Info-mode-map "l" 'Info-last)
  ;; Find non-conflicting binding for this
  ;; (evil-define-key 'motion Info-mode-map "n" 'evil-search-next)
  )
(add-hook 'Info-mode-hook #'info-binds)

;; (autoload 'enable-paredit-mode "paredit" "Turn on pseudo-structural editing of Lisp code." t)
(defun paredit-stuff ()
  (evil-define-key 'visual lisp-mode-map (kbd "SPC ;") 'paredit-comment-dwim)
  (enable-paredit-mode)
  (evil-paredit-mode))

;; =C-u C-u M-x geiser-eval-last-sexp= does this,
;; but without the open-line
(defun geiser-eval-print-last-sexp ()
  (interactive)
  ;; this works, but opens the line after the inserted text
  ;; I think I instead wants a `#:' inserted before the output,
  ;; at least in those schemes which support it.
  (open-line 1)
  (geiser-eval-last-sexp t))

(hook-envs
 #'paredit-stuff
 '(emacs-lisp-mode-hook
   eval-expression-minibuffer-setup-hook
   ielm-mode-hook
   lisp-mode-hook
   lisp-interaction-mode-hook
   scheme-mode-hook))

;;; These shouldn't bind to paredit-mode-map,
;;; but rather to their modes local maps.
;;; Alternatively a bind this globaly to the
;;; paredit-mode-map, and only define special
;;; eval functions for each major mode
(add-hook
 'lisp-interaction-mode-hook
 (lambda ()
   (define-key paredit-mode-map (kbd "C-j")
     'eval-print-last-sexp)))

;; Let's pretend any scheme buffer is an interaction scheme buffer!
;; geiser-eval-last-sexp doesn't like guile reader extensions ("#")
(add-hook
 'scheme-mode-hook
 (lambda ()
   (define-key paredit-mode-map (kbd "C-j")
     'geiser-eval-print-last-sexp)
   (define-key paredit-mode-map (kbd "C-S-j")
     'geiser-eval-last-sexp)))

;; geiser-repl-mode

;;; Can I somehow enable this for all available modes?
(hook-envs #'hs-minor-mode
           '(emacs-lisp-mode-hook
             scheme-mode-hook
             lisp-mode-hook
             c-mode-hook))

;; Geiser only looks at these, if this list is here 
(setq geiser-active-implementations '(guile racket))
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


(require 'evil-org-agenda)


(evil-org-set-key-theme '(navigation insert textobjects additional calendar))
(evil-org-agenda-set-keys)

(setq cfw:org-overwrite-default-keybinding t)

(setq custom-file "~/.emacs.d/custom.el")
(load custom-file 'noerror)
