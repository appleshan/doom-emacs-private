;;; +bindings.el --- description -*- lexical-binding: t; -*-

;;
;; Keybindings
;;
(map!
  (:after chinese
    "C-s-\\" #'chinese/evil-toggle-input-method)

  :when (featurep! :feature evil +everywhere)
  :after dired
  :map dired-mode-map
  :n "DEL" #'dired-up-directory
  )

(provide '+bindings)

;;; +bindings.el ends here