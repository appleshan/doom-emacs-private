;;; +ui.el --- description -*- lexical-binding: t; -*-

;; Theme
(setq doom-theme 'doom-city-lights)

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
(setq doom-font (font-spec :family "JetBrainsMono" :size 14 :weight 'light)
      doom-variable-pitch-font (font-spec :family "Source Sans Pro" :size 14)
      doom-unicode-font (font-spec :family "IPAmjMincho")
      doom-big-font (font-spec :family "等距更纱黑体 SC" :size 16))

;; {{ 字体设置的新方法
;; @See https://manateelazycat.github.io/emacs/2020/04/02/org-font.html
;; 利用更纱黑体这个字体来解决表格对齐的问题，因为更纱黑体字体通过融合现有字体
;; 实现中文字符的宽度刚好是英文字符宽度的两倍，以此来解决表格对齐的问题。
; (let ((emacs-font-size 14)
;       (emacs-font-name "WenQuanYi Micro Hei Mono"))
;   (set-frame-font (format "%s-%s" (eval emacs-font-name) (eval emacs-font-size)))
;   (set-fontset-font (frame-parameter nil 'font) 'unicode (eval emacs-font-name)))

(with-eval-after-load 'org
  (defun org-buffer-face-mode-variable ()
    (interactive)
    (make-face 'width-font-face)
    (set-face-attribute 'width-font-face nil :font "等距更纱黑体 SC 15")
    (setq buffer-face-mode-face 'width-font-face)
    (buffer-face-mode))

  (add-hook 'org-mode-hook 'org-buffer-face-mode-variable))

;; 上面配置的意思是，默认Emacs使用文泉驿字体，Org-Mode使用更纱黑体字体，
;; 这样既可以解决Org-Mode表格对齐问题，又避免对Emacs其他模式产生影响。
;; }}

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