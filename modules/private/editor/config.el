;;; config.el -*- lexical-binding: t; -*-

(def-package! visual-regexp
  ; :commands (vr/replace vr/query-replace) ; See the bind of init-visual-regexp-steroids.
  :defer t)

(def-package! visual-regexp-steroids
  :commands (vr/select-replace vr/select-query-replace)
  :defer t
  :bind (("C-M-%" . vr/replace)
         ("M-%"   . vr/query-replace)
         ("C-M-r"   . vr/isearch-backward)
         ("C-M-s"   . vr/isearch-forward)
         ("C-s" . isearch-forward)  ; ordinary forward search
         ("C-r" . isearch-backward) ; ordinary backward search
         ("C-c m" . vr/mc-mark)  ; for multiple-cursors
         ))

(def-package! real-auto-save
  :config
  (setq real-auto-save-interval 10) ;; in seconds
  (add-hook 'org-mode-hook 'real-auto-save-mode)
  (add-hook 'prog-mode-hook 'real-auto-save-mode))
