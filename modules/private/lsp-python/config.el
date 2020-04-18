;;; config.el -*- lexical-binding: t; -*-

;; * Python
(use-package! lsp-python
  :commands (lsp-python-enable)
  :init (add-hook! python-mode #'lsp-python-enable)
  :config
  (setq python-indent-guess-indent-offset-verbose nil)
  (set-company-backend! '(python-mode) '(company-lsp company-files company-yasnippet))
  (set-lookup-handlers! 'python-mode
    :definition #'lsp-ui-peek-find-definitions
    :references #'lsp-ui-peek-find-references))
