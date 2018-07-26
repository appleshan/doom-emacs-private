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
