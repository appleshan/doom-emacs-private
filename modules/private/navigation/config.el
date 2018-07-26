;;; config.el -*- lexical-binding: t; -*-

(after! dired
  ;; Show directory first
  ;; @see http://oremacs.com/2015/01/13/dired-options/
  ;; 传给 ls 的参数:
  ;; l: Is the only mandatory one.
  ;; a: Means to list invisible files.
  ;; G: Don't show group information. These days, when there are more laptops
  ;;    than people, the group info is rarely useful.
  ;; h: Human readable sizes, such as M for mebibytes.
  ;; 1v: Affects the sorting of digits, hopefully in a positive way.
  ;; u: sort by access time, newest first
  ;; --group-directories-first: self-explanatory, I like to have the directories
  ;; on the top, separate from the files.
  (setq dired-listing-switches "--group-directories-first -alh1vu")

  (setq directory-free-space-args "-Phk") ;目录空间选项
  (setq dired-auto-revert-buffer t)

  (setq find-ls-option '("-print0 | xargs -0 ls -ald" . ""))

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

(after! dired-k
  (setq dired-k-padding 1)
  (setq dired-k-human-readable t))

;; dired renaming like GUI file manager
(def-package! dired-efap)

(after! projectile
  ;; global ignores
  (add-to-list 'projectile-globally-ignored-files ".DS_Store")
  ;; always ignore .class / .pyc files
  (add-to-list 'projectile-globally-ignored-file-suffixes ".class")
  (add-to-list 'projectile-globally-ignored-file-suffixes ".pyc"))

(map!
  :when (featurep! :feature evil +everywhere)
  :after dired
  :map dired-mode-map
  :n "DEL" #'dired-up-directory
  :n "R" #'dired-efap
  :n "Z" #'+dired|get-dir-size)
