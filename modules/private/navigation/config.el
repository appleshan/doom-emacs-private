;;; config.el -*- lexical-binding: t; -*-

(after! projectile
  ;; global ignores
  (add-to-list 'projectile-globally-ignored-files ".DS_Store")
  ;; always ignore .class / .pyc files
  (add-to-list 'projectile-globally-ignored-file-suffixes ".class")
  (add-to-list 'projectile-globally-ignored-file-suffixes ".pyc"))
