(require 'cl)
(require 'package)
(add-to-list 'package-archives '(melpa . "http://melpa.milkbox.net/packages/") t)

;;; * TODO
;;; ** Status bar
;;; - show name of character under cursor
;;; ** Ivy
;;; - show bindings when M-x

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
	;magit ; git thing
	;; Project interaction
	;; I probably really want this.
	;; But not currently
	;projectile
	;;vimish-fold
	which-key ; show possible keys
	xresources-theme

	paredit
	geiser
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

(mapc #'require required-packages)

;;; ------------------------------------------------------------

;; (define-globalized-minor-mode
;;   global-text-scale-mode
;;   text-scale-mode
;;   (lambda () (text-scale-mode 1))) 

;;; Minor modes
(evil-mode)
(ivy-mode)
;; (linum-mode)
;; (global-linum-mode)
(which-key-mode) ; Show possible next keys after some key presses 
(show-paren-mode)
(column-number-mode)

(menu-bar-mode 0)
(tool-bar-mode 0)
(scroll-bar-mode -1)
;; "Symbol's function definition is void: text-scale-mode"
;;(text-scale-mode 1)
(text-scale-set -1)

(define-key evil-normal-state-map "\C-u" 'evil-scroll-up)
;; (define-key evil-normal-state-map (string ?\n) 'evil-open-below)

(defun prettify-scheme ()
  (setq prettify-symbols-alist
	'(("lambda" . #x3bb)
	  ("<=" . #x2264)
	  (">=" . #x2265)
	  ("sum" . #x2211)
	  ("prod" . #x220f))))
(add-hook 'scheme-mode-hook #'prettify-scheme)

(global-prettify-symbols-mode 1)

;; 'info-keys
;; '(("l" info-last)
;;   ("n" evil-search-next))

(defun info-binds ()
  (print "entering info mode")
  ;;(evil-define-key 'motion info-mode-map "l" 'info-last)
  (define-key evil-motion-state-map "l" 'info-last)
  )
;; is this for info reader of for texinfo editing
(add-hook 'Info-mode-hook #'info-binds)

;; this doesn't work at all
(define-key evil-motion-state-map "l" 'info-last)

;;(define-key evil-motion-state-map "l" 'info-last)

;; (info-mode-map
;;  (?l info-last)
;;  )

;; (define-key evil-motion-state-map (kbd "SPC ;") 'paredit-comment-dwim)
;; (define-key evil-visual-state-map (kbd "SPC ;") 'paredit-comment-dwim)
;; evil-visual-state-local-map

;; (autoload 'enable-paredit-mode "paredit" "Turn on pseudo-structural editing of Lisp code." t)
(defun paredit-stuff ()
  ;; this is global, I don't want global
  ;; (define-key evil-visual-state-local-map (kbd "SPC ;") 'paredit-comment-dwim)
  ;; this doesn't work
  (evil-define-key 'visual lisp-mode-map (kbd "SPC ;") 'paredit-comment-dwim)
  (enable-paredit-mode))


(add-hook 'emacs-lisp-mode-hook       #'paredit-stuff)
(add-hook 'eval-expression-minibuffer-setup-hook #'paredit-stuff)
(add-hook 'ielm-mode-hook             #'paredit-stuff)
(add-hook 'lisp-mode-hook             #'paredit-stuff)
;; paredit overrides eval-print-lastt-sexp
(add-hook 'lisp-interaction-mode-hook #'paredit-stuff)
(add-hook 'scheme-mode-hook           #'paredit-stuff)
;; (add-hook 'emacs-lisp-mode-hook       'evil-paredit-mode)
;; (add-hook 'eval-expression-minibuffer-setup-hook 'evil-paredit-mode)
;; (add-hook 'ielm-mode-hook             'evil-paredit-mode)
;; (add-hook 'lisp-mode-hook             'evil-paredit-mode)
;; (add-hook 'lisp-interaction-mode-hook 'evil-paredit-mode)
;; (add-hook 'scheme-mode-hook           'evil-paredit-mode)

;; geiser-repl-mode

(setq inhibit-startup-screen t)

;; Geiser only looks at these, if this list is here 
(setq geiser-active-implementations '(guile))
(setq geiser-repl-history-filename
      "/home/hugo/.emacs.d/geiser/history")

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (evil-paredit geiser paredit xresources-theme which-key ivy evil-org evil))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
