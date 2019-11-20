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
  :init
  (defface awesome-tab-unselected
    '((t (:inherit font-lock-string-face)))
    "Face used for unselected tabs." :group 'awesome-tab)
  (defface awesome-tab-selected
    '((t (:inherit font-lock-type-face :weight ultra-bold :width semi-expanded
                 :foreground "green3" :overline "green3")))
    "Face used for the selected tab." :group 'awesome-tab)
  (setq awesome-tab-cycle-scope 'tabs)
  :config
  (awesome-tab-mode t))

;;
;; Keybindings
;;

(map! :leader
  (:desc "help" :prefix "h"
    :n "h" help-map
    :desc "Discover Emacs major mode"              :nv "z"  #'discover-my-major)
  )
