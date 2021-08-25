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

;;;###autoload
(defun +programming|ascii-table ()
  "Display basic ASCII table (0 thru 128)."
  (interactive)
  (switch-to-buffer "*ASCII*")
  (erase-buffer)
  (setq buffer-read-only nil)        ;; Not need to edit the content, just read mode (added)
  (local-set-key "q" 'bury-buffer)   ;; Nice to have the option to bury the buffer (added)
  (setq lower32 '("nul" "soh" "stx" "etx" "eot" "enq" "ack" "bel"
                  "bs" "ht" "lf" "vt" "ff" "cr" "so" "si"
                  "dle" "dc1" "dc2" "dc3" "dc4" "nak" "syn" "etb"
                  "can" "em" "sub" "esc" "fs" "gs" "rs" "us"
                  ))
  (save-excursion (let ((i -1))
    (insert "ASCII characters 0 thru 127.\n\n")
    (insert " Hex  Dec  Char|  Hex  Dec  Char|  Hex  Dec  Char|  Hex  Dec  Char\n")
    (while (< i 31)
      (insert (format "%4x %4d %4s | %4x %4d %4s | %4x %4d %4s | %4x %4d %4s\n"
                      (setq i (+ 1  i)) i (elt lower32 i)
                      (setq i (+ 32 i)) i (single-key-description i)
                      (setq i (+ 32 i)) i (single-key-description i)
                      (setq i (+ 32 i)) i (single-key-description i)))
      (setq i (- i 96))))))
