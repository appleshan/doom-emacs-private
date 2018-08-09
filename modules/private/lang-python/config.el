;;; config.el -*- lexical-binding: t; -*-

(after! pipenv
  (setq pipenv-projectile-after-switch-function
        #'pipenv-projectile-after-switch-extended)
  (map! :map pipenv-mode-map
        :localleader
        :desc "Pipenv" :prefix "v"
        :desc "Activcate":nv "a" #'pipenv-activate
        :desc "Deactivate" :nv "d" #'pipenv-deactivate
        :desc "Pipenv Install" :nv "i" #'pipenv-install
        :desc "Pipenv Uninstall" :nv "u" #'pipenv-uninstall
        :desc "Pipenv Open" :nv "o" #'pipenv-open
        :desc "Pipenv Run" :nv "r" #'pipenv-run
        :desc "Pipenv Shell" :nv "s" #'pipenv-shell
        ))

(def-package! blacken
  :commands blacken-mode
  :init
  (add-hook 'python-mode-hook 'blacken-mode))
