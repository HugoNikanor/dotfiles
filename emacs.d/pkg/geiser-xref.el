
;;;###autoload
(defun geiser--xref-backend () 'geiser)

(cl-defmethod xref-backend-identifier-at-point ((_backend (eql geiser)))
  (geiser--symbol-at-point))

;;; Return completion table
(cl-defmethod xref-backend-identifier-completion-table ((_backend (eql geiser)))
  geiser-completion-symbol-list-func)

;; TODO should this do (xref-push-marker-stack (point-marker))?
(cl-defmethod xref-backend-definitions ((_backend (eql geiser)) symbol)
  (let ((cmd  `(:eval (:ge symbol-location
                           ',(if (stringp  symbol)
                                 (intern symbol)
                               symbol)))))
    (let ((loc (geiser-eval--retort-result (geiser-eval--send/wait cmd))))
      (list (xref-make (or (geiser-edit--location-name loc) "No Summary")
                       (xref-make-file-location
                        (geiser-edit--location-file loc)
                        ;; Geiser finds stuff by searching, see
                        ;; geiser-edit--try-edit-location &
                        ;; geiser-edit--goto-line
                        (or (geiser-edit--location-line loc) 0)
                        (or (geiser-edit--location-column loc) 1)
                        )
                       )))))


;;; All symbols that match pattern (symbol?)
;; Currently errors with:
;; cl-no-applicable-method: No applicable method: xref-item-location, "make-arbiter"
(cl-defmethod xref-backend-apropos ((_backend (eql geiser)) prefix-str)
  ;; (geiser-edit--symbol-re symbol)
  (geiser-eval--send/result `(:eval (:ge completions ,prefix-str))))

(add-hook 'geiser-mode-hook
          (lambda () (add-hook 'xref-backend-functions #'geiser--xref-backend nil)))

(provide 'geiser-xref)
