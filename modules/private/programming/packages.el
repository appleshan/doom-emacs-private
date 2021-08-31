;;; packages.el -*- lexical-binding: t; -*-

(package! aggressive-indent)

(package! magit-pretty-graph
  :recipe (:host github :repo "georgek/magit-pretty-graph" :files (:defaults "*")))

(package! magit-delta)

(package! symbol-overlay)

(package! color-rg
  :recipe (:host github :repo "manateelazycat/color-rg" :files (:defaults "*")))
