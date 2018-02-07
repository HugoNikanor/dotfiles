(require 'cl)
(require 'package)
(add-to-list 'package-archives '(melpa . "http://melpa.milkbox.net/packages/") t)

;;; * TODO
;;; ** Status bar
;;; - show name of character under cursor
;;; ** Ivy
;;; - show bindings when M-x
;;; ** Evil
;;; :vsp <file>
;;; :vsplit <file> works, but the short version doesn't

(setq required-packages
      '(
	;; calendar framework
	;calfw
	;calfw-org

	;; Complete anything
	;company
	;counsel
	;counsel-projectile

	evil ; /good/ vim keybinds
	;evil-magit
	evil-org
	;evil-vimish-fold
	;flx
	;; on the fly checking
	;flycheck
	;; General keybinds
	;general
	ivy ; fuzzy finder
	magit ; git thing
	magit
	evil-magit
	;; Project interaction
	;; I probably really want this.
	;; But not currently
	;projectile
	;;vimish-fold
	which-key ; show possible keys
	xresources-theme

	paredit
	geiser

	haskell-mode
	;sql
	;flymake
	;auto-complete
	popup
	lorem-ipsum
	))

(package-initialize)

(defun packages-installed-p ()
  "Returns t if all packages are installed
   nil otherwise."
  (loop for p in required-packages
	when (not (package-installed-p p)) do (return nil)
	finally (return t)))

(unless (packages-installed-p)
  ;; Refresh
  (message "%s" "Refreshing package lists...")
  (package-refresh-contents)
  (message "%s" "done.")
  ;; Install
  (dolist (p required-packages)
    (when (not (package-installed-p p))
      (package-install p))))

;;; xresources-theme-color: Window system is not in use or not initialized
;;; xresources-theme needs to be removed from required-packages if emacs
;;; should be able to be started from a terminal.
(mapc #'require required-packages)

;;; ------------------------------------------------------------

(evil-mode)
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
;; (define-key evil-normal-state-map (string ?\n) 'evil-open-below)

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
  (enable-paredit-mode))

;; =C-u C-u M-x geiser-eval-last-sexp= does this,
;; but without the open-line
(defun geiser-eval-print-last-sexp ()
  (interactive)
  (open-line 1)	; this works, but opens the line after the inserted text
  (geiser-eval-last-sexp t))

(add-hook 'emacs-lisp-mode-hook       #'paredit-stuff)
(add-hook 'eval-expression-minibuffer-setup-hook #'paredit-stuff)
(add-hook 'ielm-mode-hook             #'paredit-stuff)
(add-hook 'lisp-mode-hook             #'paredit-stuff)
(add-hook 'lisp-interaction-mode-hook #'paredit-stuff)
(add-hook 'lisp-interaction-mode-hook
	  (lambda () (define-key paredit-mode-map (kbd "C-j") 'eval-print-last-sexp)))
(add-hook 'scheme-mode-hook           #'paredit-stuff)
;; Let's pretend any scheme buffer is an interaction scheme buffer!
;; geiser-eval-last-sexp doesn't like guile reader extensions ("#")
(add-hook 'scheme-mode-hook
	  ;; C-j fungerar inte att binda här.
	  ;; C-l är tillfälligt eftersom det fungerar...
	  ;; C-k går inte heller att binda...
	  (lambda ()
	    (define-key scheme-mode-map (kbd "C-l") 'geiser-eval-print-last-sexp)
	    (define-key scheme-mode-map (kbd "C-k") 'geiser-eval-last-sexp)))

;; geiser-repl-mode

;; Geiser only looks at these, if this list is here 
(setq geiser-active-implementations '(guile racket))
;; geiser doesn't seem to find this file,
;; and is thereby not able to write to it.
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

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("065efdd71e6d1502877fd5621b984cded01717930639ded0e569e1724d058af8" default)))
 '(package-selected-packages
   (quote
    (lorem-ipsum auto-complete sql-mode SqlMode evil-magit magit haskell-mode evil-paredit geiser paredit xresources-theme which-key ivy evil-org evil))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
