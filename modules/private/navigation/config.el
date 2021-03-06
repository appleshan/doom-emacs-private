;;; config.el -*- lexical-binding: t; -*-

(after! dired
  ;; Show directory first
  ;; @see http://oremacs.com/2015/01/13/dired-options/
  (setq dired-listing-switches "--group-directories-first -alh1vu") ;传给 ls 的参数

  (setq directory-free-space-args "-Phk") ;目录空间选项
  (setq dired-auto-revert-buffer t)

  (setq find-ls-option '("-print0 | xargs -0 ls -ald" . ""))

  (add-hook 'dired-mode-hook #'(lambda ()
    (dired-hide-details-mode -1) ;进入时显示详细信息
    (define-key evil-normal-state-local-map (kbd "<tab>") #'dired-hide-details-mode)
    ))

  ;; @see http://oremacs.com/2015/01/12/dired-file-size/
  (defun +dired|get-dir-size ()
    (interactive)
    (let ((files (dired-get-marked-files)))
      (with-temp-buffer
        (apply 'call-process "/usr/bin/du" nil t nil "-sh" files)
        (message
          "Size of all marked files: %s"
          (progn
            (re-search-backward "\\(^[ 0-9.,]+[A-Za-z]+\\).*$")
            (match-string 1))))))
  )

(after! dired-x
  :config
  (defun enable-dired-omit-mode ()
    (setq dired-omit-files (concat dired-omit-files "\\|^\\..+$")) ; omit all hidden file which starts with `.'
    (dired-omit-mode 1)) ; initially omit unintrested files
  (add-hook 'dired-mode-hook 'enable-dired-omit-mode))

;; Highlights dired buffer like k
(after! dired-k
  (setq dired-k-padding 1)
  (setq dired-k-human-readable t))

;; dired renaming like GUI file manager
(use-package! dired-efap)

(use-package! dired-single
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

(map!
  :when (featurep! :feature evil +everywhere)
  :after dired
  :n [(f5)] #'dired-single-magic-buffer
  :n [(meta f5)] #'dired-single-toggle-buffer-name
  :n [(shift f5)] #'+dired|show-current-dir
  :n [(control f5)] #'+dired|single-magic-buffer
  :map dired-mode-map
  :n [return] #'dired-single-buffer
  :n [mouse-1] #'dired-single-buffer-mouse
  :n "^" #'dired-single-up-directory
  :n "DEL" #'dired-single-up-directory
  :n "R" #'dired-efap
  :n "Z" #'+dired|get-dir-size)
