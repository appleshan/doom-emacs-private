;;; packages.el -*- lexical-binding: t; -*-

(package! aggressive-indent)

(package! auto-sudoedit)

(package! company-english-helper
  :recipe (:host github :repo "manateelazycat/company-english-helper" :files (:defaults "*")))

(package! multiple-cursors)

;; 可视化正则匹配
;; https://github.com/benma/visual-regexp.el
;; https://github.com/benma/visual-regexp-steroids.el
(package! visual-regexp)
(package! visual-regexp-steroids)

(package! real-auto-save)
