;;; config.el -*- lexical-binding: t; -*-

;; company-mode
(after! company
  (setq company-selection-wrap-around t
        ;; do or don't automatically start completion after <idle time>
        company-idle-delay 0.5
        ;; at least 1 letters need to be there though
        company-minimum-prefix-length 1
        ;; show completion numbers for hotkeys
        company-show-numbers t
        ;; align annotations to the right
        company-tooltip-align-annotations t
        company-search-regexp-function #'company-search-flex-regexp)
  )

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
