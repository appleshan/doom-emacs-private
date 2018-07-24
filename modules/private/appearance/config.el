;;; config.el -*- lexical-binding: t; -*-

(def-package! on-screen
  :init (require 'on-screen)
  :config (on-screen-global-mode +1))
