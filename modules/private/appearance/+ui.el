;;; +ui.el --- description -*- lexical-binding: t; -*-

;; Theme
(setq doom-theme 'doom-dracula)

;; Change cursor color depending on mode
(setq evil-emacs-state-cursor `("red" hbar))     ; _
(setq evil-normal-state-cursor `("green" box))   ; █
(setq evil-visual-state-cursor `("orange" box))  ; █
(setq evil-insert-state-cursor `("red" bar))     ; ⎸
(setq evil-replace-state-cursor `("red" bar))
(setq evil-operator-state-cursor `("red" hollow))
(setq evil-motion-state-cursor `("orange" box))  ; █

;; Prevents some cases of Emacs flickering
(add-to-list 'default-frame-alist '(inhibit-double-buffering . t))

;; Fonts
(setq doom-font (font-spec :family "JetBrainsMono" :size 18 :weight 'light)
      doom-variable-pitch-font (font-spec :family "JetBrainsMono" :size 14)
      doom-unicode-font (font-spec :family "Source Han Sans")
      doom-big-font (font-spec :family "JetBrainsMono" :size 20))

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

;; Hide the mouse while typing:
(setq make-pointer-invisible t)

(when (not (featurep! :ui doom-quit))
  (setq confirm-kill-emacs nil))

(provide '+ui)

;;; +ui.el ends here