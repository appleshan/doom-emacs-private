;;; config.el -*- lexical-binding: t; -*-

(def-package! alert
  :config
  (when (eq system-type 'darwin)
    (setq alert-default-style 'notifier))
  (when (eq system-type 'gnu/linux)
    (setq alert-default-style 'notifications)))

;; The all-seeing eye of Sauron
(def-package! sauron
  ;:commands (sauron-toggle-hide-show)
  ;:bind ("M-o" . sauron-toggle-hide-show)
  :init
  (progn
    (when (eq system-type 'gnu/linux)
      ;; Remove mu4e if on linux
      (require 'sauron)
      (setq sauron-modules '(sauron-org sauron-notifications)))

    (setq sauron-max-line-length 120
          sauron-watch-patterns '("dakrone" "thnetos" "okenezak")
          sauron-watch-nicks '("dakrone" "thnetos")
          sauron-nick-insensitivity 20
          sauron-frame-geometry "120x36+0+0")
    ;; filter out IRC spam
    (defun tsp/hide-irc-user-spam (origin priority msg &optional properties)
      (or (string-match "^*** Users" msg)))
    (defun tsp/hide-tweet-counts (origin priority msg &optional properties)
      (or (string-match "^[0-9]+ new tweets" msg)))
    (add-hook 'sauron-event-block-functions #'tsp/hide-irc-user-spam)
    (add-hook 'sauron-event-block-functions #'tsp/hide-tweet-counts)

    (sauron-start-hidden)
    ;; Need to stop tracking notifications, because sauron will be sending
    ;; notifications!
    (sauron-notifications-stop)
    (add-hook 'sauron-event-added-functions 'sauron-alert-el-adapter))
  :config
  (progn
    ;; Add the unread sauron notification count to the modeline
    (add-to-list 'global-mode-string '(cdr (sauron-count-events)))

    (defun eos/compilation-finish (buffer msg)
      "Send a sauron notification for compilation completing"
      (interactive)
      (sauron-add-event 'compilation
                        3
                        (format "[%s]: %s" buffer msg)
                        (lambda () (switch-to-buffer-other-window "*compilation*"))
                        nil))
    (add-to-list 'compilation-finish-functions #'eos/compilation-finish)

    (defun finish ()
      "Generic function for signaling something is \"done\"."
      (interactive)
      (sauron-add-event major-mode
                        3
                        (concat "Finished command in " (buffer-name))
                        (lambda () (switch-to-buffer-other-window (buffer-name)))
                        nil))))
