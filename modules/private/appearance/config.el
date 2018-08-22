;;; config.el -*- lexical-binding: t; -*-

;; ** loading
(load! "+ui")
(load! "+popup")

(def-package! doom-modeline
  :defer t
  :hook (after-init . doom-modeline-init))

;; Discover key bindings and their meaning for the current Emacs major mode
(def-package! discover-my-major)

(def-package! on-screen
  :init (require 'on-screen)
  :config (on-screen-global-mode +1))


;;
;; Keybindings
;;

(map! :leader
  (:desc "help" :prefix "h"
    :n "h" help-map
    :desc "Discover Emacs major mode"              :nv "z"  #'discover-my-major)
  )
