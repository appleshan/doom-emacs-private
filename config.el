;;; ~/.doom.d/config.el -*- lexical-binding: t; -*-

(when (fboundp 'set-charset-priority)
  (set-charset-priority 'emacs))

;; User Credentials
(setq
    user-mail-address "apple.shan@gmail.com"
    user-full-name "Apple Shan")
(setq auth-sources '("~/.authinfo"))

(provide 'config)

;;; config.el ends here