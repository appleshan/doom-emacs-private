;;; config.el -*- lexical-binding: t; -*-

(use-package! aggressive-indent
  :config
  (dolist (hook (list
                 'emacs-lisp-mode-hook
                 'lisp-interaction-mode-hook
                 'lisp-mode-hook
                 'java-mode-hook
                 'sh-mode-hook
                 'js2-mode-hook
                 'js-mode-hook
                 'html-mode-hook
                 'css-mode-hook
                 'go-mode-hook
                 'slime-repl-mode-hook
                 'cmake-mode-hook
                 'web-mode-hook
                 ))
    (add-hook hook (lambda () (aggressive-indent-mode 1))))

  ;; Disable aggressive indent in some mode.
  (dolist (hook (list
                 'python-mode-hook
                 ))
    (add-hook hook (lambda () (aggressive-indent-mode -1))))
  )

(after! flycheck
  (global-flycheck-mode -1)
  ;; I don't like `global-flycheck-mode', some mode, such as elisp mode don't need.
  (dolist (hook (list
                 'python-mode-hook
                 ))
    (add-hook hook '(lambda () (flycheck-mode 1)))))

;; pretty-magit
;; @see http://www.modernemacs.com/post/pretty-magit/
(after! magit
  ;; {{ C-c C-a to amend without any prompt
  (defun magit-just-amend ()
    (interactive)
    (save-window-excursion
      (shell-command "git --no-pager commit --amend --reuse-message=HEAD")
      (magit-refresh)))

  (define-key magit-status-mode-map (kbd "C-c C-a") 'magit-just-amend)
  ;; }}

  (use-package! pretty-magit
    :defer 1
    :load-path "~/.config/doom/local/pretty-magit/"
    :config
    (pretty-magit "Feature" ? '(:foreground "slate gray" :height 1.0 :family "FontAwesome"))
    (pretty-magit "Add" ? '(:foreground "#375E97" :height 1.0 :family "FontAwesome"))
    (pretty-magit "Fix" ? '(:foreground "#FB6542" :height 1.0 :family "FontAwesome"))
    (pretty-magit "Clean" ? '(:foreground "#FFBB00" :height 1.0 :family "FontAwesome"))
    (pretty-magit "Docs" ? '(:foreground "#3F681C" :height 1.0 :family "FontAwesome"))
    (pretty-magit "master" ? '(:box nil :height 1.0 :family "github-octicons") t)
    (pretty-magit "origin" ? '(:box nil :height 1.0 :family "github-octicons") t)))

;; Package 'magit-delta' integrates Delta (https://github.com/dandavison/delta)
;; with Magit, so that diffs in Magit are displayed with color highlighting
;; provided by Delta.
(use-package! magit-delta
  :if (locate-file "delta" exec-path exec-suffixes 1)
  :after magit
  :defer 1
  :config
  (setq magit-delta-delta-args
        `("--plus-color" "#016000"
          "--plus-emph-color" "#02a000"
          "--minus-color" "#840001"
          "--minus-emph-color" "#b60004"
          "--max-line-distance" "0.6"
          "--24-bit-color" ,(if xterm-color--support-truecolor
                                "always"
                              "never")
          "--color-only"))
  (magit-delta-mode +1))

;; A pretty git graph drawn with Emacs lisp.
;; M-x magit-pg-repo says bye-bye to the server
(use-package! magit-pretty-graph
  :defer 1)

;; symbol-overlay 高亮同一个symbol,并对其编辑
(use-package! symbol-overlay
  :defer t
  :diminish symbol-overlay-mode
  :hook ((prog-mode html-mode yaml-mode conf-mode) . symbol-overlay-mode)
  :custom-face
  (symbol-overlay-default-face ((t (:inherit 'background-color :underline t))))
  :bind ("C-$" . symbol-overlay-put)
  :config
  (setq symbol-overlay-map (make-sparse-keymap)))

(after! projectile
  (setq projectile-require-project-root t))

(after! yasnippet
  ;; Remove Yasnippet's default tab key binding
  (define-key yas-minor-mode-map (kbd "<tab>") nil)
  (define-key yas-minor-mode-map (kbd "TAB") nil)
  ;; Alternatively use Control-c + tab
  (define-key yas-minor-mode-map (kbd "\C-c TAB") 'yas-expand)

  (set-face-background 'secondary-selection "gray"))

;; @See https://github.com/lujun9972/emacs-document/blob/master/emacs-common/Smartparens%E7%94%A8%E6%B3%95%E8%AF%A6%E8%A7%A3.org
(after! smartparens
  (defmacro def-pairs (pairs)
    "Define functions for pairing. PAIRS is an alist of (NAME . STRING)
  conses, where NAME is the function name that will be created and
  STRING is a single-character string that marks the opening character.

    (def-pairs ((paren . \"(\")
                (bracket . \"[\"))

  defines the functions WRAP-WITH-PAREN and WRAP-WITH-BRACKET,
  respectively."
    `(progn
       ,@(loop for (key . val) in pairs
               collect
               `(defun ,(read (concat
                               "wrap-with-"
                               (prin1-to-string key)
                               "s"))
                    (&optional arg)
                  (interactive "p")
                  (sp-wrap-with-pair ,val)))))

  (def-pairs ((paren . "(")
              (bracket . "[")
              (brace . "{")
              (single-quote . "'")
              (double-quote . "\"")
              (back-quote . "`")))

  (bind-keys
   :map smartparens-mode-map
   ("C-M-a" . sp-beginning-of-sexp)
   ("C-M-e" . sp-end-of-sexp)

   ("C-c C-<down>" . sp-down-sexp)
   ("C-c C-<up>"   . sp-up-sexp)
   ("C-c M-<down>" . sp-backward-down-sexp)
   ("C-c M-<up>"   . sp-backward-up-sexp)

   ("C-M-f" . sp-forward-sexp)
   ("C-M-b" . sp-backward-sexp)

   ("C-M-n" . sp-next-sexp)
   ("C-M-p" . sp-previous-sexp)

   ("C-S-f" . sp-forward-symbol)
   ("C-S-b" . sp-backward-symbol)

   ("C-c C-<right>" . sp-forward-slurp-sexp)
   ("C-c C-<left>"  . sp-backward-slurp-sexp)
   ("C-c M-<right>" . sp-forward-barf-sexp)
   ("C-c M-<left>"  . sp-backward-barf-sexp)

   ("C-M-t" . sp-transpose-sexp)
   ("C-M-k" . sp-kill-sexp)
   ("C-k"   . sp-kill-hybrid-sexp)
   ("M-k"   . sp-backward-kill-sexp)
   ("C-M-w" . sp-copy-sexp)

   ("C-M-d" . delete-sexp)

   ("M-<backspace>" . backward-kill-word)
   ("C-<backspace>" . sp-backward-kill-word)
   ([remap sp-backward-kill-word] . backward-kill-word)

   ("M-[" . sp-backward-unwrap-sexp)
   ("M-]" . sp-unwrap-sexp)

   ("C-x C-t" . sp-transpose-hybrid-sexp)

   ("C-c ("  . wrap-with-parens)
   ("C-c ["  . wrap-with-brackets)
   ("C-c {"  . wrap-with-braces)
   ("C-c '"  . wrap-with-single-quotes)
   ("C-c \"" . wrap-with-double-quotes)
   ("C-c _"  . wrap-with-underscores)
   ("C-c `"  . wrap-with-back-quotes))
  )

;;; color-rg: Search and refactoring tool based on ripgrep.
(use-package! color-rg
  :commands (color-rg-search-input-in-project
             color-rg-search-symbol-in-project
             color-rg-search-input-in-current-file
             color-rg-search-symbol-in-current-file)
  :config
  ;; https://emacs.stackexchange.com/a/10588/22102
  (evil-make-overriding-map color-rg-mode-map 'normal)
  ;; force update evil keymaps after git-timemachine-mode loaded
  (add-hook 'color-rg-mode-hook #'evil-normalize-keymaps)

  (setq color-rg-kill-temp-buffer-p nil)
  (remove-hook 'compilation-filter-hook
               #'doom-apply-ansi-color-to-compilation-buffer-h)

  (custom-set-faces!
  `(color-rg-font-lock-match :foreground ,(doom-color 'red))
  `(color-rg-font-lock-function-location :foreground ,(doom-color 'magenta))
  `(color-rg-font-lock-header-line-text :foreground ,(doom-color 'dark-cyan))
  `(color-rg-font-lock-header-line-keyword :foreground ,(doom-color 'magenta))
  `(color-rg-font-lock-header-line-edit-mode :foreground ,(doom-color 'magenta))))

;; hideshow 扩展: 显示被隐藏的代码行数
;; @See https://github.com/condy0919/emacs-newbie/blob/master/introduction-to-builtin-modes.md#hideshow
(after! hideshow
  ;; 这里额外启用了 :box t 属性使得提示更加明显
  (defconst hideshow-folded-face '((t (:inherit 'font-lock-comment-face :box t))))

  (defun hideshow-folded-overlay-fn (ov)
      (when (eq 'code (overlay-get ov 'hs))
        (let* ((nlines (count-lines (overlay-start ov) (overlay-end ov)))
               (info (format " ... #%d " nlines)))
          (overlay-put ov 'display (propertize info 'face hideshow-folded-face)))))

  (setq hs-set-up-overlay 'hideshow-folded-overlay-fn))
