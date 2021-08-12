;;; config.el -*- lexical-binding: t; -*-

(after! flycheck
  (global-flycheck-mode -1)
  ;; I don't like `global-flycheck-mode', some mode, such as elisp mode don't need.
  (dolist (hook (list
                 'python-mode-hook
                 ))
    (add-hook hook '(lambda () (flycheck-mode 1)))))

;; pretty-magit
;; @see http://www.modernemacs.com/post/pretty-magit/
(after! magit
  (use-package! pretty-magit
    :load-path "~/.config/doom/local/pretty-magit/"
    :config
    (pretty-magit "Feature" ? '(:foreground "slate gray" :height 1.0 :family "FontAwesome"))
    (pretty-magit "Add" ? '(:foreground "#375E97" :height 1.0 :family "FontAwesome"))
    (pretty-magit "Fix" ? '(:foreground "#FB6542" :height 1.0 :family "FontAwesome"))
    (pretty-magit "Clean" ? '(:foreground "#FFBB00" :height 1.0 :family "FontAwesome"))
    (pretty-magit "Docs" ? '(:foreground "#3F681C" :height 1.0 :family "FontAwesome"))
    (pretty-magit "master" ? '(:box nil :height 1.0 :family "github-octicons") t)
    (pretty-magit "origin" ? '(:box nil :height 1.0 :family "github-octicons") t)))

(after! magithub
  (setq magithub-clone-default-directory "~/projects-private"))

;; A pretty git graph drawn with Emacs lisp.
;; M-x magit-pg-repo says bye-bye to the server
(use-package! magit-pretty-graph)

(after! projectile
  (setq projectile-require-project-root t))

(after! yasnippet
  ;; Remove Yasnippet's default tab key binding
  (define-key yas-minor-mode-map (kbd "<tab>") nil)
  (define-key yas-minor-mode-map (kbd "TAB") nil)
  ;; Alternatively use Control-c + tab
  (define-key yas-minor-mode-map (kbd "\C-c TAB") 'yas-expand)

  (set-face-background 'secondary-selection "gray"))

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
