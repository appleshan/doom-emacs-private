;;; autoload.el -*- lexical-binding: t; -*-

;;;###autoload
(defun imenu-list-toggle ()
  (interactive)
  ;; fix conflicts with treemacs pop-up
  (if (featurep! :ui treemacs)
      (pcase (treemacs-current-visibility)
        ('visible (delete-window (treemacs-get-local-window)))))
  (imenu-list-smart-toggle))
