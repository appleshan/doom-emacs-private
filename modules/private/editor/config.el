;;; config.el -*- lexical-binding: t; -*-

(use-package! aggressive-indent
  :config
  (dolist (hook (list
                 'emacs-lisp-mode-hook
                 'lisp-interaction-mode-hook
                 'lisp-mode-hook
                 'java-mode-hook
                 'sh-mode-hook
                 'js2-mode-hook
                 'js-mode-hook
                 'html-mode-hook
                 'css-mode-hook
                 'go-mode-hook
                 'slime-repl-mode-hook
                 'cmake-mode-hook
                 'web-mode-hook
                 ))
    (add-hook hook (lambda () (aggressive-indent-mode 1))))

  ;; Disable aggressive indent in some mode.
  (dolist (hook (list
                 'python-mode-hook
                 ))
    (add-hook hook (lambda () (aggressive-indent-mode -1))))
  )

;; 这个包会自动检测是否有权限编辑，没有权限就自动调用sudo.
(use-package! auto-sudoedit
  :config
  ;; Just hook on `find-file-hook', don't hook `dired-mode-hook', it's unnecessary.
  (add-hook 'find-file-hook (lambda () (auto-sudoedit-mode 1))))

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

;; 英文自动补全和翻译，激活命令toggle-company-english-helper
(use-package! company-english-helper
  :defer t
  :commands (toggle-company-english-helper)
  :init
  (map! :leader
        :prefix ("y" . "Translate")
        "M" #'toggle-company-english-helper)
  :config
  (setq company-english-helper-fuzz-search-p t))

(use-package! visual-regexp
  ; :commands (vr/replace vr/query-replace) ; See the bind of init-visual-regexp-steroids.
  :defer t)

(use-package! visual-regexp-steroids
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

(use-package! real-auto-save
  :config
  (setq real-auto-save-interval 10) ;; in seconds
  (add-hook 'org-mode-hook 'real-auto-save-mode)
  (add-hook 'prog-mode-hook 'real-auto-save-mode))

(map! :nv "C-u" #'evil-scroll-up

      (:after company
        (:map company-active-map
          "C-j" #'company-select-next
          "C-k" #'company-select-previous))
)
