;;; +ui.el --- description -*- lexical-binding: t; -*-

;; Theme
(when (featurep! :ui doom)
  (setq doom-theme 'doom-dracula))

;; Change cursor color depending on mode
(setq evil-emacs-state-cursor `("red" hbar))     ; _
(setq evil-normal-state-cursor `("green" box))   ; █
(setq evil-visual-state-cursor `("orange" box))  ; █
(setq evil-insert-state-cursor `("red" bar))     ; ⎸
(setq evil-replace-state-cursor `("red" bar))
(setq evil-operator-state-cursor `("red" hollow))
(setq evil-motion-state-cursor `("orange" box))  ; █

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
(add-hook! '(minibuffer-setup-hook doom-popup-mode-hook)
  (setq-local show-trailing-whitespace nil))

;; Smooth mouse scrolling
(setq mouse-wheel-scroll-amount '(2 ((shift) . 1))  ; scroll two lines at a time
      mouse-wheel-progressive-speed nil             ; don't accelerate scrolling
      mouse-wheel-follow-mouse t                    ; scroll window under mouse
      scroll-step 1)

;; Happy hacking apple!
;; 用 Emacs, 需忘记鼠标, 无视菜单.
(with-current-buffer (get-buffer-create "*scratch*")
  (emacs-lisp-mode)
  (insert ";; Talk is cheap. Show me the code.\n\n"))

;; Display visited file's path in the frame title
;; @See http://emacsredux.com/blog/2013/04/07/display-visited-files-path-in-the-frame-title/
(setq frame-title-format
      `((:eval (if (buffer-file-name)
                   (abbreviate-file-name (buffer-file-name))
                 "%b"))))

(when (not (featurep! :ui doom-quit))
  (setq confirm-kill-emacs nil))

(provide '+ui)

;;; +ui.el ends here