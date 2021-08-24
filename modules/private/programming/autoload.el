;;; autoload.el -*- lexical-binding: t; -*-

;;;###autoload
(defhydra +hydra/symbol-overlay (:hint nil)
  ("i" symbol-overlay-put                  "toggle at point" :column "highlight")
  ("t" symbol-overlay-toggle-in-scope      "toggle scope   "                    )
  ("u" symbol-overlay-remove-all           "unhighlight all"                    )
  ("n" symbol-overlay-jump-next            "next           " :column "move     ")
  ("p" symbol-overlay-jump-prev            "prev           "                    )
  ("<" symbol-overlay-jump-first           "first          "                    )
  (">" symbol-overlay-jump-last            "last           "                    )
  ("f" symbol-overlay-switch-forward       "forward        " :column "others   ")
  ("b" symbol-overlay-switch-backward      "backward       "                    )
  ("y" symbol-overlay-save-symbol          "copy           " :column "action   ")
  ("Y" symbol-overlay-rename               "rename         "                    )
  ("w" symbol-overlay-query-replace        "query replace  "                    )
  ("." symbol-overlay-jump-to-definition   "to definition  "                    ))

;;;###autoload
(defun find-symbol-at-point ()
  "Find the symbol at point, i.e. go to definition."
  (interactive)
  (let ((sym (symbol-at-point)))
    (if (boundp sym)
        (find-variable sym)
      (find-function sym))))
