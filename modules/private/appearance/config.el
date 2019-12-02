;;; config.el -*- lexical-binding: t; -*-

;; ** loading
(load! "+ui")
(load! "+popup")

;; Discover key bindings and their meaning for the current Emacs major mode
(def-package! discover-my-major)

(def-package! on-screen
  :init (require 'on-screen)
  :config (on-screen-global-mode +1))

(def-package! awesome-tab
  :config
  (set-face-attribute 'awesome-tab-default nil
    :height 1.1)
  (set-face-attribute 'awesome-tab-selected nil
    :foreground "#839496"
    :overline "#002b36")
  (set-face-attribute 'awesome-tab-unselected nil
    :foreground "#002b36"
    ;; :background "#839496"
    :background "#d9d9d9"
    :overline "#839496")
  (define-key awesome-tab-mode-map awesome-tab-prefix-key nil)
  (setq awesome-tab-background-color "#2e3434"
        awesome-tab-cycle-scope 'tabs ; Navigate through visible tabs only.
        awesome-tab-style "wave" ; alternate
        awesome-tab-height 22
        awesome-tab-label-fixed-length 0
        awesome-tab-display-sticky-function-name nil)
  (awesome-tab-mode t)
  (with-eval-after-load 'evil
    (define-key evil-normal-state-map (kbd ",tt") 'awesome-tab-switch-group)
    (define-key evil-normal-state-map (kbd ",ta") 'awesome-tab-select-beg-tab)
    (define-key evil-normal-state-map (kbd ",te") 'awesome-tab-select-end-tab)
    (define-key evil-normal-state-map (kbd ",t<") 'awesome-tab-move-current-tab-to-left)
    (define-key evil-normal-state-map (kbd ",t>") 'awesome-tab-move-current-tab-to-right)
    (define-key evil-normal-state-map (kbd ",tl") 'awesome-tab-forward)
    (define-key evil-normal-state-map (kbd ",th") 'awesome-tab-backward))
  )

;;
;; Keybindings
;;

(map! :leader
  (:desc "help" :prefix "h"
    :n "h" help-map
    :desc "Discover Emacs major mode"              :nv "z"  #'discover-my-major)
  )
