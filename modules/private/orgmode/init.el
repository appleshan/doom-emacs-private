;;; config.el -*- lexical-binding: t; -*-

;; Files and directory variables
(setq org-directory (expand-file-name "~/projects-private/org-notes/")
      org-gtd-dir (expand-file-name "~/projects-private/org-gtd/"))

;; lang/org
(setq org-agenda-files (list org-gtd-dir)

      ;; The standard unicode characters are usually misaligned depending on the
      ;; font. This bugs me. Personally, markdown #-marks for headlines are more
      ;; elegant.
      org-bullets-bullet-list '("#"))
