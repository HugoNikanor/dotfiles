;;; babellang.el --- Babel Lang
;;; Commentary:
;;; Code:


;;;###autoload
(defun babel-lang ()
  "Set ispell language from babel usepackage"
  (defvar lang)

  (with-current-buffer (current-buffer)
   (progn
     (setf (point) 0)
     (re-search-forward "\\\\usepackage\\[\\([^]]*\\)\\]{babel}")
     (setq lang (match-string-no-properties 1))
     (ispell-change-dictionary lang))))



(provide 'babel-lang)
;;; babel-lang.el ends here
