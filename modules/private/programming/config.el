;;; config.el -*- lexical-binding: t; -*-

(after! flycheck
  (global-flycheck-mode -1)
  ;; I don't like `global-flycheck-mode', some mode, such as elisp mode don't need.
  (dolist (hook (list
                 'python-mode-hook
                 ))
    (add-hook hook '(lambda () (flycheck-mode 1)))))

;; pretty-magit
;; @see http://www.modernemacs.com/post/pretty-magit/
(after! magit
  (use-package! pretty-magit
    :load-path "~/.config/doom/local/pretty-magit/"
    :config
    (pretty-magit "Feature" ? '(:foreground "slate gray" :height 1.0 :family "FontAwesome"))
    (pretty-magit "Add" ? '(:foreground "#375E97" :height 1.0 :family "FontAwesome"))
    (pretty-magit "Fix" ? '(:foreground "#FB6542" :height 1.0 :family "FontAwesome"))
    (pretty-magit "Clean" ? '(:foreground "#FFBB00" :height 1.0 :family "FontAwesome"))
    (pretty-magit "Docs" ? '(:foreground "#3F681C" :height 1.0 :family "FontAwesome"))
    (pretty-magit "master" ? '(:box nil :height 1.0 :family "github-octicons") t)
    (pretty-magit "origin" ? '(:box nil :height 1.0 :family "github-octicons") t)))

;; A pretty git graph drawn with Emacs lisp.
;; M-x magit-pg-repo says bye-bye to the server
(use-package! magit-pretty-graph)

;; symbol-overlay 高亮同一个symbol,并对其编辑
(use-package! symbol-overlay
  :defer t
  :diminish symbol-overlay-mode
  :hook ((prog-mode html-mode yaml-mode conf-mode) . symbol-overlay-mode)
  :custom-face
  (symbol-overlay-default-face ((t (:inherit 'background-color :underline t))))
  :bind ("C-$" . symbol-overlay-put)
  :config
  (setq symbol-overlay-map (make-sparse-keymap)))

(after! projectile
  (setq projectile-require-project-root t))

(after! yasnippet
  ;; Remove Yasnippet's default tab key binding
  (define-key yas-minor-mode-map (kbd "<tab>") nil)
  (define-key yas-minor-mode-map (kbd "TAB") nil)
  ;; Alternatively use Control-c + tab
  (define-key yas-minor-mode-map (kbd "\C-c TAB") 'yas-expand)

  (set-face-background 'secondary-selection "gray"))
