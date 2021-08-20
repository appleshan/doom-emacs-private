;;; config.el -*- lexical-binding: t; -*-

(use-package! wakatime-mode
  :init
  (let* ((wakatime-cfg "~/.wakatime.cfg")
         (wakatime-bin-name "wakatime")
         (wakatime-bin (locate-file wakatime-bin-name exec-path)))
    (if (and wakatime-bin
             (file-exists-p wakatime-bin)
             (file-exists-p wakatime-cfg))
        (progn
          (setq-default wakatime-cli-path wakatime-bin)
          (setq-default wakatime-api-key
                        (cadr (s-match "api_key\\s-*=\\s-*\\(.*\\)\\s-*"
                                       (with-temp-buffer
                                         (insert-file-contents wakatime-cfg)
                                         (buffer-string)))))
          (add-hook 'org-mode-hook 'wakatime-mode)
          (add-hook 'prog-mode-hook 'wakatime-mode)
          ))))

(defun +service/wakatime-dashboard ()
  (interactive)
  (browse-url "http://wakatime.com/dashboard"))


;;
;; Keybindings
;;

(map!
  :leader
  :desc "Wakatime dashboard" "ow" #'+service/wakatime-dashboard
  )
