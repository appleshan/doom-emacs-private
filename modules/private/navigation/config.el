;;; config.el -*- lexical-binding: t; -*-

(after! dired
  ;; @see http://oremacs.com/2015/01/13/dired-options/
  ;; l: Is the only mandatory one.
  ;; a: Means to list invisible files.
  ;; G: Don't show group information. These days, when there are more laptops
  ;;    than people, the group info is rarely useful.
  ;; h: Human readable sizes, such as M for mebibytes.
  ;; 1v: Affects the sorting of digits, hopefully in a positive way.
  ;; --group-directories-first: self-explanatory, I like to have the directories
  ;;                            on the top, separate from the files.
  (setq dired-listing-switches "--group-directories-first -alh1vu") ;传给 ls 的参数

  (setq directory-free-space-args "-Phk") ;目录空间选项
  ;; Instantly revert Dired buffers on re-visiting them, with no message.
  (setq dired-auto-revert-buffer t)

  (setq find-ls-option '("-print0 | xargs -0 ls -ald" . ""))

  (add-hook 'dired-mode-hook #'(lambda ()
    (dired-hide-details-mode -1) ;进入时显示详细信息
    (define-key evil-normal-state-local-map (kbd "<tab>") #'dired-hide-details-mode)
    )))

;; Feature `dired-x' provides extra `dired' functionality.
(after! dired-x
  :config
  (defun enable-dired-omit-mode ()
    (setq dired-omit-files (concat dired-omit-files "\\|^\\..+$")) ; omit all hidden file which starts with `.'
    (dired-omit-mode 1)) ; initially omit unintrested files
  (add-hook 'dired-mode-hook 'enable-dired-omit-mode))

;; dired renaming like GUI file manager
(use-package! dired-efap
  :after-call dired
  :defer 2)

(use-package! dired-single
  :after-call dired
  :defer 1
  :config
  (defun +dired|single-magic-buffer ()
    (interactive)
    (dired-single-magic-buffer default-directory))
  (defun +dired|show-current-dir ()
    (interactive)
    (message "Current directory is: %s" default-directory)))

(after! dumb-jump
  ;; If your project has multi-line method signatures you should use ag.
  (setq dumb-jump-force-searcher 'rg)
  (setq dumb-jump-prefer-searcher 'rg))

(after! projectile
  ;; global ignores
  (add-to-list 'projectile-globally-ignored-files ".DS_Store")
  ;; always ignore .class / .pyc files
  (add-to-list 'projectile-globally-ignored-file-suffixes ".class")
  (add-to-list 'projectile-globally-ignored-file-suffixes ".pyc"))

(after! ace-window
  ;; Customizing ace-window leading char
  ;; @See https://oremacs.com/2015/02/27/ace-window-leading-char/
  (custom-set-faces
   '(aw-leading-char-face
     ((t :height 3.5 :foreground "deep sky blue" :inherit 'aw-leading-char-face))))
  )

;; Never lose your place in Emacs again!
(use-package! dogears
  :load-path "~/.config/doom/local/dogears.el/"
  ;; These bindings are optional, of course:
  :bind (:map global-map
              ("M-g d" . dogears-go)
              ("M-g M-b" . dogears-back)
              ("M-g M-f" . dogears-forward)
              ("M-g M-d" . dogears-list)
              ("M-g M-D" . dogears-sidebar)))

(map!
  :after dired
  :map dired-mode-map
  :n [return] #'dired-single-buffer
  :n [mouse-1] #'dired-single-buffer-mouse
  :n "^" #'dired-single-up-directory
  :n "DEL" #'dired-single-up-directory
  :n "R" #'dired-efap
  :n "Z" #'+dired|get-dir-size)
