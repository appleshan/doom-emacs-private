;;; ~/.doom.d/init.base.el -*- lexical-binding: t; -*-
;; Copy me to ~/.doom.d/init.el or ~/.config/doom/init.el, then edit me!

(doom! :feature
       (evil +everywhere)

       :completion
       (ivy +fuzzy)

       :ui
       doom
       doom-modeline

       :emacs
       dired
       eshell

       :lang
       emacs-lisp

       :config
       (default +bindings +snippets +evil-commands)
)
