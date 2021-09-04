;;; ~/.config/doom/config.el -*- lexical-binding: t; -*-

(setq user-full-name "Apple Shan"
      user-mail-address "apple.shan@gmail.com")

;; When I bring up Doom's scratch buffer with SPC x, it's often to play with
;; elisp or note something down (that isn't worth an entry in my org files). I
;; can do both in `lisp-interaction-mode'.
(setq doom-scratch-initial-major-mode 'lisp-interaction-mode)

;; Line numbers are pretty slow all around. The performance boost of
;; disabling them outweighs the utility of always keeping them on.
(setq display-line-numbers-type nil)

;; IMO, modern editors have trained a bad habit into us all: a burning need for
;; completion all the time -- as we type, as we breathe, as we pray to the
;; ancient ones -- but how often do you *really* need that information? I say
;; rarely. So opt for manual completion:
(setq company-idle-delay nil)

;; Disable invasive lsp-mode features
(setq lsp-ui-sideline-enable nil   ; not anymore useful than flycheck
      lsp-ui-doc-enable nil        ; slow and redundant with K
      lsp-enable-symbol-highlighting nil
      ;; If an LSP server isn't present when I start a prog-mode buffer, you
      ;; don't need to tell me. I know. On some systems I don't care to have a
      ;; whole development environment for some ecosystems.
      +lsp-prompt-to-install-server 'quiet)

;; Implicit /g flag on evil ex substitution, because I less often want the
;; default behavior.
(setq evil-ex-substitute-global t)

(add-load-path! "~/projects-private/doom-snippets")

;;
;;; Modules

;;; :editor evil
;; Focus new window after splitting
(setq evil-split-window-below t
      evil-vsplit-window-right t)

;;; :tools magit
(setq magit-repository-directories '(("~/projects-private" . 2))
      ;; Don't restore the wconf after quitting magit, it's jarring
      magit-inhibit-save-previous-winconf t
      transient-values '((magit-rebase "--autosquash" "--autostash")
                         (magit-pull "--rebase" "--autostash")))

;; Unbindings
(global-unset-key (kbd "C-SPC"))

;;; config.el ends here
