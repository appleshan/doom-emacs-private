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

;; Required by `tabbar-ruler'
(def-package! mode-icons
  :defer t
  :commands mode-icons-mode
  :config
  (mode-icons-mode 1))

;; Required by `tabbar-ruler'
(def-package! tabbar)

(def-package! tabbar-ruler
  :init
  (setq tabbar-ruler-tab-height 28)
  (setq tabbar-ruler-fancy-tab-separator 'contour)
  (setq tabbar-ruler-global-tabbar t)
  (setq tabbar-ruler-global-ruler nil)
  (setq tabbar-ruler-popup-menu t)
  (setq tabbar-ruler-popup-toolbar t)
  (setq tabbar-ruler-popup-scrollbar t)
  :config
  (tabbar-ruler-group-by-projectile-project))

;;
;; Keybindings
;;

(map! :leader
  (:desc "help" :prefix "h"
    :n "h" help-map
    :desc "Discover Emacs major mode"              :nv "z"  #'discover-my-major)
  )
