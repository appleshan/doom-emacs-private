;;; config.el -*- lexical-binding: t; -*-

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
  (use-package! pretty-magit
    :load-path "~/.config/doom/local/pretty-magit/"
    :config
    (pretty-magit "Feature" ? '(:foreground "slate gray" :height 1.0 :family "FontAwesome"))
    (pretty-magit "Add" ? '(:foreground "#375E97" :height 1.0 :family "FontAwesome"))
    (pretty-magit "Fix" ? '(:foreground "#FB6542" :height 1.0 :family "FontAwesome"))
    (pretty-magit "Clean" ? '(:foreground "#FFBB00" :height 1.0 :family "FontAwesome"))
    (pretty-magit "Docs" ? '(:foreground "#3F681C" :height 1.0 :family "FontAwesome"))
    (pretty-magit "master" ? '(:box nil :height 1.0 :family "github-octicons") t)
    (pretty-magit "origin" ? '(:box nil :height 1.0 :family "github-octicons") t)))

;; A pretty git graph drawn with Emacs lisp.
;; M-x magit-pg-repo says bye-bye to the server
(use-package! magit-pretty-graph)

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

   ("C-<down>" . sp-down-sexp)
   ("C-<up>"   . sp-up-sexp)
   ("M-<down>" . sp-backward-down-sexp)
   ("M-<up>"   . sp-backward-up-sexp)

   ("C-M-f" . sp-forward-sexp)
   ("C-M-b" . sp-backward-sexp)

   ("C-M-n" . sp-next-sexp)
   ("C-M-p" . sp-previous-sexp)

   ("C-S-f" . sp-forward-symbol)
   ("C-S-b" . sp-backward-symbol)

   ("C-<right>" . sp-forward-slurp-sexp)
   ("C-<left>"  . sp-backward-slurp-sexp)
   ("M-<right>" . sp-forward-barf-sexp)
   ("M-<left>"  . sp-backward-barf-sexp)

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
