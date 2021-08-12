;;; ~/.config/doom/init.el -*- lexical-binding: t; -*-

;; This file controls what Doom modules are enabled and what order they load
;; in. Remember to run 'doom sync' after modifying it!

;; NOTE Press 'SPC h d h' (or 'C-h d h' for non-vim users) to access Doom's
;;      documentation. There you'll find a "Module Index" link where you'll find
;;      a comprehensive list of Doom's modules and what flags they support.

;; NOTE Move your cursor over a module's name (or its flags) and press 'K' (or
;;      'C-c c k' for non-vim users) to view its documentation. This works on
;;      flags as well (those symbols that start with a plus).
;;
;;      Alternatively, press 'gd' (or 'C-c c d') on a module to browse its
;;      directory (for easy access to its source code).

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

(doom! :completion
       (company +childframe)
       ;; ivy
       (helm +childframe +fuzzy)
       ;;ido
       ;;vertico

       :ui
       ;;deft
       doom
       doom-dashboard
       ;;doom-quit
       ;;(emoji +unicode)
       ;;fill-column
       hl-todo
       ;;hydra
       ;;indent-guides     ; highlighted indent columns
       ;;ligatures
       ;;minimap
       (modeline +light)
       ;;nav-flash
       ;;neotree
       ophints
       (popup +defaults)
       ;;treemacs
       ;;tree-sitter
       ;;unicode
       ;;tabs
       vc-gutter
       vi-tilde-fringe
       ;;window-select
       workspaces
       zen

       :input
       ;;chinese
       ;;japanese
       ;;layout

       :editor
       (evil +everywhere)
       file-templates
       fold              ; (nigh) universal code folding
       ;;objed
       format            ; automated prettiness
       ;;lispy             ; vim for lisp, for people who dont like vim
       multiple-cursors  ; editing in many places at once
       ;;parinfer          ; turn lisp into python, sort of
       rotate-text       ; cycle region at point between text candidates
       snippets
       ;;word-wrap

       :emacs
       dired             ; making dired pretty [functional]
       electric          ; smarter, keyword-based electric-indent
       ibuffer           ; interactive buffer management
       ;;undo
       vc

       :term
       ;;eshell            ; a consistent, cross-platform shell (WIP)
       ;;shell
       ;;term              ; terminals in Emacs
       ;;vterm

       :checkers
       syntax
       ;;spell
       ;;grammar

       :tools
       ;;ansible
       ;;(debugger +lsp)
       ;;direnv
       ;;docker
       editorconfig      ; let someone else argue about tabs vs spaces
       ;;ein               ; tame Jupyter notebooks with emacs
       (eval +overlay)
       ;;gist
       (lookup +docsets +dictionary)
       ;;lsp
       ;;macos             ; MacOS-specific commands
       magit             ;
       ;;make              ; run make tasks from Emacs
       ;;pass                ; password manager for nerds
       ;;pdf               ; pdf enhancements
       ;;prodigy           ; FIXME managing external services & code builders
       ;;rgb               ; creating color strings
       ;;taskrunner        ; taskrunner for all your projects
       ;;terraform         ; infrastructure as code
       ;;tmux              ; an API for interacting with tmux
       ;;upload            ; map local to remote projects via ssh/ftp

       :os
       ;;arch
       ;;(:if IS-MAC macos)
       ;;nixos
       ;;tty               ; enable terminal integration

       :lang
       ;;agda
       ;;assembly
       ;;beancount
       ;;(cc +lsp)
       ;;crystal
       ;;clojure
       ;;(csharp +unity +lsp)
       ;;common-lisp
       ;;coq
       ;;data
       ;;(dart +lsp)
       ;;erlang
       ;;elixir
       ;;elm
       emacs-lisp
       ;;ess
       ;;faust
       ;;fsharp            ; ML stands for Microsoft's Language
       ;;go
       ;;haskell
       ;;hy
       ;;(java +meghanada)
       ;;(javascript +lsp)
       ;;julia
       ;;latex
       ;;ledger
       ;;(lua +fennel)
       ;;markdown
       ;;nim
       ;;nix
       ;;ocaml
       (org +dragndrop +journal +roam +present)
       ;;perl
       ;;php
       ;;plantuml
       ;;purescript
       ;;(python +lsp)
       ;;qt
       ;;racket
       ;;rest
       ;;ruby
       ;;(rust +lsp)
       ;;scala
       ;;(scheme +guile)
       sh
       ;;sml
       ;;swift
       ;;web
       yaml

       :email
       ;;(mu4e +gmail)       ; WIP
       ;;notmuch             ; WIP
       ;;(wanderlust +gmail) ; WIP

       :app
       ;;calendar
       ;;everywhere
       ;;irc
       ;;(rss +org)
       ;;ereader

       :config
       ;;literate
       (default +bindings +smartparens)

       ;;------------- Load Personal Modules --------------
       :private
      ;core
      ;appearance
      ;navigation
      ;notify
      ;chinese
      ;editor
      ;orgmode
      ;programming
      ;lang-python
      ;lang-lsp
      ;lsp-python
      ;service
       )

;(setq custom-file (expand-file-name "local/custom.el" doom-emacs-dir))
;(load custom-file 'no-error 'no-message)

(provide 'init)

;;; init.el ends here
