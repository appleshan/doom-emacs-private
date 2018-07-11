;;; +ui.el --- description -*- lexical-binding: t; -*-

;; Theme
(when (featurep! :ui doom)
  (setq doom-theme 'doom-dracula))

;; Start maximized
(add-to-list 'default-frame-alist '(fullscreen . maximized))

;; Fonts
(setq
      doom-font (font-spec :family "Fira Mono" :size 16)
      doom-variable-pitch-font (font-spec :family "Fira Sans")
      doom-unicode-font (font-spec :family "DejaVu Sans Mono")
      doom-big-font (font-spec :family "Fira Mono" :size 19))

;; Modeline
(setq +doom-modeline-buffer-file-name-style 'relative-from-project)

;; Minibuffer
(setq show-trailing-whitespace t)
(add-hook! minibuffer-setup (setq-local show-trailing-whitespace nil))

(provide '+ui)

;;; +ui.el ends here