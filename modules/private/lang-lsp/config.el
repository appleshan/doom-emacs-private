;;; config.el -*- lexical-binding: t; -*-

(def-package! lsp-mode
  :commands (lsp-mode lsp-define-stdio-client)
  :config
  (setq lsp-enable-eldoc nil) ; 禁止eldoc
  (setq lsp-response-timeout 25)
  (evil-set-command-property 'lsp-goto-type-definition :jump t)
  (evil-set-command-property 'lsp-goto-implementation :jump t)
  )

(def-package! lsp-ui
  :after lsp-mode
  :hook (lsp-mode . lsp-ui-mode)
  :init
  (setq-default lsp-ui-doc-frame-parameters
                '((left . -1)
                  (top . -1)
                  (no-accept-focus . t)
                  (min-width . 0)
                  (width . 0)
                  (min-height . 0)
                  (height . 0)
                  (internal-border-width . 5)
                  (vertical-scroll-bars)
                  (horizontal-scroll-bars)
                  (left-fringe . 0)
                  (right-fringe . 0)
                  (menu-bar-lines . 0)
                  (tool-bar-lines . 0)
                  (line-spacing . 0.1)
                  (unsplittable . t)
                  (undecorated . t)
                  (minibuffer . nil)
                  (visibility . nil)
                  (mouse-wheel-frame . nil)
                  (no-other-frame . t)
                  (cursor-type)
                  (no-special-glyphs . t)))
  :config
  (setq lsp-ui-doc-include-signature nil)  ; don't include type signature in the child frame

  (setq lsp-ui-peek-expand-function (lambda (xs) (mapcar #'car xs)))

  (evil-make-overriding-map lsp-ui-peek-mode-map 'normal)
  (define-key lsp-ui-peek-mode-map (kbd "h") 'lsp-ui-peek--select-prev-file)
  (define-key lsp-ui-peek-mode-map (kbd "l") 'lsp-ui-peek--select-next-file)
  (define-key lsp-ui-peek-mode-map (kbd "j") 'lsp-ui-peek--select-next)
  (define-key lsp-ui-peek-mode-map (kbd "k") 'lsp-ui-peek--select-prev)

  (define-key lsp-ui-mode-map (kbd "C-c l") 'lsp-ui-imenu)
  (define-key lsp-ui-mode-map [remap xref-find-definitions] #'lsp-ui-peek-find-definitions)
  (define-key lsp-ui-mode-map [remap xref-find-references] #'lsp-ui-peek-find-references)

  (setq lsp-ui-sideline-enable nil
        lsp-enable-completion-at-point t
        lsp-ui-doc-position 'at-point
        lsp-ui-doc-header nil
        lsp-ui-doc-enable t
        lsp-ui-doc-border (doom-color 'fg))
  )

(def-package! company-lsp
  :after lsp-mode
  :init
  ;; Language servers have better idea filtering and sorting,
  ;; don't filter results on the client side.
  (setq company-transformers nil
        company-lsp-async t
        company-lsp-cache-candidates nil)
  :config
  (set! :company-backend 'lsp-mode '(company-lsp))
  (setq company-lsp-enable-recompletion t))
