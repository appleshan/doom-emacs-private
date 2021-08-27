;;; packages.el -*- lexical-binding: t; -*-

(package! aggressive-indent)

(package! magit-pretty-graph
  :recipe (:host github :repo "georgek/magit-pretty-graph" :files (:defaults "*")))

(package! symbol-overlay)
