;;; formfeed.el --- Formfeed mode
;;; Commentary:
;;; Code:

(require 'evil)

(defun insert-formfeed ()
  "Insert a formfeed at current location."
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

;;;###autoload
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

(provide 'formfeed)
;;; formfeed.el ends here
