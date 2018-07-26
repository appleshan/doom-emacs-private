;;; config.el -*- lexical-binding: t; -*-

;; 绑定扩展名到特定的模式
(dolist (elt-cons '((".*rc\\'" . conf-mode)
                    ("\\.myclirc\\'" . conf-mode)
                    (".xprofile'" . conf-mode)
                    ("torrc'" . conf-mode)
                    ("\\.lrc\\'" . emms-lyrics-mode)
                    ("\\.org\\'" . org-mode)
                    ("\\.cron\\(tab\\)?\\'" . crontab-mode)
                    ("cron\\(tab\\)?\\." . crontab-mode)))
  (add-to-list 'auto-mode-alist elt-cons))

;; {{ Move Current Line Up or Down
;; @see http://emacsredux.com/blog/2013/04/02/move-current-line-up-or-down/
(defun +editor|move-line-up ()
  "Move up the current line."
  (interactive)
  (transpose-lines 1)
  (forward-line -2)
  (indent-according-to-mode))

(defun +editor|move-line-down ()
  "Move down the current line."
  (interactive)
  (forward-line 1)
  (transpose-lines 1)
  (forward-line -1)
  (indent-according-to-mode))

(global-set-key (kbd "M-<up>") '+editor|move-line-up)
(global-set-key (kbd "M-<down>") '+editor|move-line-down)
;; }}
