;;; +avy.el --- description -*- lexical-binding: t; -*-

(defhydra +hydra/avy-jump-char (:color blue :columns 3 :hint nil)
  "Jump By Char ->"
  ("c" avy-goto-char "Char")
  ("l" avy-goto-char-in-line "In line")
  ("t" avy-goto-char-timer "Timer")
  ("2" avy-goto-char-2 "Char2")
  ("a" avy-goto-char-2-above "Above")
  ("b" avy-goto-char-2-below "Below")
  )

(defhydra +hydra/avy-jump-line (:color blue :columns 3 :hint nil)
  "Jump To Line ->"
  ("u" avy-goto-line-above "Above")
  ("d" avy-goto-line-below "Below")
  ("s" avy-goto-line "Line Start")
  ("e" avy-goto-end-of-line "Line End")
  )

(defhydra +hydra/avy-jump-word (:color blue :columns 3 :hint nil)
  "Jump By Word ->"
  ("w" avy-goto-word-1 "word1")
  ("0" avy-goto-word-0 "word0")
  ("a" avy-goto-word-0-above "above-0")
  ("A" avy-goto-word-1-above "above-1")
  ("b" avy-goto-word-0-below "below0")
  ("B" avy-goto-word-1-below "below1")
  ("o" avy-goto-word-or-subword-1 "word or subword")
  ("s" avy-goto-subword-0 "subword-0")
  ("S" avy-goto-subword-1 "subword-1")
  )

(provide '+avy)

;;; +avy.el ends here