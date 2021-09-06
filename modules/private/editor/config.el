;;; config.el -*- lexical-binding: t; -*-

;; 这个包会自动检测是否有权限编辑，没有权限就自动调用sudo.
(use-package! auto-sudoedit
  :defer t
  :config
  ;; Just hook on `find-file-hook', don't hook `dired-mode-hook', it's unnecessary.
  (add-hook 'find-file-hook (lambda () (auto-sudoedit-mode 1))))

;; company-mode
(after! company
  (setq company-selection-wrap-around t
        ;; do or don't automatically start completion after <idle time>
        company-idle-delay 0.5
        ;; at least 1 letters need to be there though
        company-minimum-prefix-length 1
        ;; show completion numbers for hotkeys
        company-show-numbers t
        ;; align annotations to the right
        company-tooltip-align-annotations t
        company-search-regexp-function #'company-search-flex-regexp)
  )

;; 英文自动补全和翻译，激活命令 toggle-company-english-helper
(use-package! company-english-helper
  :defer t
  :commands (toggle-company-english-helper)
  :init
  (map! :leader
        :prefix ("y" . "Translate")
        :desc "Toggle company english" "t" #'toggle-company-english-helper)
  :config
  (setq company-english-helper-fuzz-search-p t))

;; 输入 insert-translated-name-insert 激活命令，可以输入中文后按空格翻译成英文插入当前位置。
(use-package! insert-translated-name
  :commands (insert-translated-name-insert)
  :init
  (map! :leader
        :prefix ("y" . "Translate")
        :desc "Insert translated name" "i" #'insert-translated-name-insert)
  :config
  (setq insert-translated-name-default-style 'origin)
  (setq insert-translated-name-translate-engine "youdao"))

(use-package! visual-regexp
  ; :commands (vr/replace vr/query-replace) ; See the bind of init-visual-regexp-steroids.
  :defer t)

(use-package! visual-regexp-steroids
  :commands (vr/select-replace vr/select-query-replace)
  :defer t
  :bind (("C-M-%" . vr/replace)
         ("M-%"   . vr/query-replace)
         ("C-M-r"   . vr/isearch-backward)
         ("C-M-s"   . vr/isearch-forward)
         ("C-s" . isearch-forward)  ; ordinary forward search
         ("C-r" . isearch-backward) ; ordinary backward search
         ("C-c m" . vr/mc-mark)  ; for multiple-cursors
         ))

(use-package! real-auto-save
  :config
  (setq real-auto-save-interval 10) ;; in seconds
  (add-hook 'org-mode-hook 'real-auto-save-mode)
  (add-hook 'prog-mode-hook 'real-auto-save-mode))

;; conf-mode: major-mdoe for editing conf files
;; https://github.com/jrockway/emacs/blob/master/lisp/textmodes/conf-mode.el
(use-package! conf-mode
  :mode ("\\.*rc$" . conf-unix-mode))

;; major made for crontab files
;; https://github.com/emacs-pe/crontab-mode
(use-package! crontab-mode
  :defer t
  :mode "\\.?cron\\(tab\\)?\\'")

;; dot-env: An Emacs major mode for .env files
;; https://github.com/preetpalS/emacs-dotenv-mode
(use-package! dotenv-mode
  :mode (("\\.env" . dotenv-mode)
         ("\\.env\\..*\\'" . dotenv-mode)))

(use-package! ini-mode
  :mode "\\.ini\\'")

;; nginx-mode: Emacs editing mode for Nginx config files
;; https://github.com/ajc/nginx-mode
(use-package! nginx-mode
  :if (executable-find "nginx")
  :mode (("/nginx/sites-\\(?:available\\|enabled\\)/" . nginx-mode)
         ("nginx.conf" . nginx-mode))
  :custom
  (nginx-indent-level 4)
  (nginx-indent-tabs-mode nil))

;; emacs mode for editing ssh config files.
;; https://github.com/jhgorrell/ssh-config-mode-el
(use-package! ssh-config-mode
  :defer t
  :mode (("/\\.ssh/config\\'"   . ssh-config-mode)
         ("/sshd?_config\\'"    . ssh-config-mode)
         ("/known_hosts\\'"     . ssh-known-hosts-mode)
         ("/authorized_keys\\'" . ssh-authorized-keys-mode))
  :hook
  (ssh-config-mode . turn-on-font-lock))

;; systemd: Major mode for editing systemd units
;; https://github.com/holomorph/systemd-mode
(use-package! systemd
  :defer t
  :mode ("\\.service\\'" . systemd-mode))

;; TOML
(use-package! toml-mode)

(map! :nv "C-u" #'evil-scroll-up

      (:after company
        (:map company-active-map
          "C-j" #'company-select-next
          "C-k" #'company-select-previous))
)
