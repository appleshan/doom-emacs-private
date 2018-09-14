;;; config.el -*- lexical-binding: t; -*-

(def-package! aggressive-indent
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

(def-package! company-english-helper
  :init (require 'company-english-helper))

(def-package! multiple-cursors
  :init
  (setq mc/list-file (concat doom-cache-dir ".mc-lists.el"))
  :bind (;; multiple-cursors
         ("C->" . mc/mark-next-like-this) ; 寻找下一个与当前“选中”（region）相匹配的文本并选中匹配并添加新光标.
         ("C-<" . mc/mark-previous-like-this) ; 类上，但是寻找方向相反.
         ("C-M->" . mc/skip-to-next-like-this)
         ("C-M-<" . mc/skip-to-previous-like-this)
         ("C-c C->" . mc/mark-all-like-this) ; 找到当前buffer中所有匹配并增加光标.
         ("C-;" . mc/mark-all-symbols-like-this-toggle)
         ("C-S-<mouse-1>" . mc/add-cursor-on-click) ; 使用鼠标点击添加/删除光标
         ;; 对多行的region的操作:
         ("C-c c c" . mc/edit-lines) ; 当前选中的region中的每一行增加一个cursor.
         ("C-c c e" . mc/edit-ends-of-lines) ; 当前选中的region中的每一行的行尾增加一个cursor.
         ("C-c c a" . mc/edit-beginnings-of-lines) ; 当前选中的region中的每一行的行首增加一个cursor.
         :map mc/keymap
         ("C-|" . mc/vertical-align-with-space)
         ("C-;" . mc/my-quit)
         ("M-n" . mc/cycle-forward)
         ("M-p" . mc/cycle-backward))
  :config
  (progn
    (defun mc/my-quit ()
      "Quit from mark mode."
      (interactive)
      (mc/keyboard-quit)
      (multiple-cursors-mode 0))

    (defun mc/mark-all-symbols-like-this-toggle ()
      "Toogle when only one matches!"
      (interactive)
      (if (region-active-p)
          (mc/my-quit)
        (mc/mark-all-symbols-like-this)))
  ))

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

(map! :nv "C-u" #'evil-scroll-up

      (:after company
        (:map company-active-map
          "C-j" #'company-select-next
          "C-k" #'company-select-previous))
)
