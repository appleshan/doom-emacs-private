;;; config.el -*- lexical-binding: t; -*-

;;{{
;; Tweak the GC threshold.
;; The default value is 800000, set it to something higher once the startup is
;; complete. And set it to something even higher in the minibuffer (for ido and
;; other complex features)
(defvar eos/default-gc-threshold (* 20 1024 1024)
  "Default `gc-cons-threshold' after startup")

;; Set to `t' to display GC messages
(setq garbage-collection-messages nil)

;; 100mb during startup
(setq gc-cons-threshold (* 100 1024 1024))

;; set back to default after startup finishes
(add-hook 'after-init-hook
          (lambda ()
            (message "Resetting garbage collection..")
            (setq gc-cons-threshold eos/default-gc-threshold)))

(defun eos/minibuffer-setup-hook ()
  (setq gc-cons-threshold (* 100 1024 1024)))

(defun eos/minibuffer-exit-hook ()
  (setq gc-cons-threshold eos/default-gc-threshold))

(add-hook 'minibuffer-setup-hook #'eos/minibuffer-setup-hook)
(add-hook 'minibuffer-exit-hook #'eos/minibuffer-exit-hook)
;;}}

(defun +core/show-current-buffer-major-mode ()
  (interactive)
  (describe-variable 'major-mode))
