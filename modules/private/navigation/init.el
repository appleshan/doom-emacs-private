;;; init.el -*- lexical-binding: t; -*-

;; Automatically create missing directories when creating new files
(defun +dired|create-non-existent-directory ()
(let ((parent-directory (file-name-directory buffer-file-name)))
  (when (and (not (file-exists-p parent-directory))
             (y-or-n-p (format "Directory `%s' does not exist! Create it?" parent-directory)))
    (make-directory parent-directory t))))
(push #'+dired|create-non-existent-directory find-file-not-found-functions)
