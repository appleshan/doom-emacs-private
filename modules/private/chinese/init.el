;;; init.el -*- lexical-binding: t; -*-

;; {{ make IME compatible with evil-mode
(defun +chinese/evil-toggle-input-method ()
  "when toggle on input method, goto evil-insert-state. "
  (interactive)

  (cond
   ((and (boundp 'evil-mode) evil-mode)
    ;; evil-mode
    (cond
     ((eq evil-state 'insert)
      (toggle-input-method))
     (t
      (evil-insert-state)
      (unless current-input-method
        (toggle-input-method))
      ))
    (if current-input-method
        (message "IME on!")
      (message "IME off!"))
    )
   (t
    ;; NOT evil-mode, some guy don't use evil-mode at all
    (toggle-input-method))))

(defadvice evil-insert-state (around evil-insert-state-hack activate)
  ad-do-it
  (if current-input-method (message "IME on!")))

(global-set-key (kbd "C-\\") '+chinese/evil-toggle-input-method)
;; }}
