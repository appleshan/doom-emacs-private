;;; ~/.doom.d/config.el -*- lexical-binding: t; -*-

;; User Credentials
(setq
    user-mail-address "apple.shan@gmail.com"
    user-full-name "Apple Shan")
(setq auth-sources '("~/.authinfo"))

;; ------------- Load Personal Modules --------------
(load! "+bindings")
(load! "+ui")

(provide 'config)

;;; config.el ends here