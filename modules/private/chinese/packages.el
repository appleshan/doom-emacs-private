;;; packages.el -*- lexical-binding: t; -*-

(package! cal-china-x)
(package! pangu-spacing)

(package! rime
  :recipe (:host github :repo "DogLooksGood/emacs-rime"
           :files ("Makefile" "lib.c" "rime.el" "rime-predicates.el")))

(package! evil-pinyin)
