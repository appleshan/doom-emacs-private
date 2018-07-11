;;; ~/.doom.d/config.el -*- lexical-binding: t; -*-

;; ------------- ELPA Mirrors --------------
;; ELPA: refer to https://elpa.emacs-china.org/
;;
;; ELPA                   镜像地址
;; GNU ELPA               http://elpa.emacs-china.com/gnu/
;; MELPA                  http://elpa.emacs-china.com/melpa/
;; MELPA Stable           http://elpa.emacs-china.com/melpa-stable/
;; Marmalade              http://elpa.emacs-china.com/marmalade/
;; Org                    http://elpa.emacs-china.com/org/
;; Sunrise Commander ELPA http://elpa.emacs-china.com/sunrise-commander/
;; user42 ELPA            http://elpa.emacs-china.com/user42/
;;
(setq package-archives
    '(
      ;; Emacs-China 开源软件镜像站
      ("melpa-cn"  . "https://elpa.emacs-china.org/melpa/")
      ("gnu-cn"    . "https://elpa.emacs-china.org/gnu/")
      ("org-cn"    . "https://elpa.emacs-china.org/org/")
    ; ("user42-cn" . "https://elpa.emacs-china.org/user42/")

      ;; 清华大学 TUNA 协会开源软件镜像站
    ; ("melpa-cn" . "https://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/")
    ; ("gnu-cn"   . "https://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/")
    ; ("org-cn"   . "https://mirrors.tuna.tsinghua.edu.cn/elpa/org/")

      ;; source elpa
    ; ("melpa" . "melpa.org/packages/")
    ; ("gnu"   . "elpa.gnu.org/packages/")
    ; ("org"   . "orgmode.org/elpa/")
      ))

;; ------------- Load Personal Modules --------------
(load! "+ui")

(provide 'config)

;;; config.el ends here