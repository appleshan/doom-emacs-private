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
;; GNU ELPA               http://elpa.emacs-china.org/gnu/
;; MELPA                  http://elpa.emacs-china.org/melpa/
;; MELPA Stable           http://elpa.emacs-china.org/stable-melpa/
;; Org                    http://elpa.emacs-china.org/org/
;;
(setq package-archives
    '(
      ;; Emacs-China 开源软件镜像站
      ("melpa-cn"  . "https://elpa.emacs-china.org/melpa/")
      ("gnu-cn"    . "https://elpa.emacs-china.org/gnu/")
      ("org-cn"    . "https://elpa.emacs-china.org/org/")

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
       (company           ; the ultimate code completion backend
        +childframe)      ; a nicer company UI. Emacs +26 only!
       ;;helm
       (ivy               ; a search engine for love and life
        +childframe
        +icons
        +prescient)
       ;;ido
       ;;vertico

       :ui
       ;;deft
       doom              ; what makes DOOM look the way it does
       doom-dashboard    ; a nifty splash screen for Emacs
       ;;doom-quit         ; DOOM quit-message prompts when you quit Emacs
       ;;(emoji +ascii +github)  ; 🙂
       hl-todo           ; highlight TODO/FIXME/NOTE/DEPRECATED/HACK/REVIEW
       hydra
       ;;indent-guides     ; highlighted indent columns
       ;;ligatures
       ;;minimap           ; show a map of the code on the side
       modeline          ; snazzy, Atom-inspired modeline, plus API
       nav-flash         ; blink cursor line after big motions
       ;;neotree           ; a project drawer, like NERDTree for vim
       ophints           ; highlight the region an operation acts on
       (popup +all +defaults)   ; tame sudden yet inevitable temporary windows
       ;;tabs              ; a tab bar for Emacs
       ;;treemacs          ; a project drawer, like neotree but cooler
       ;;unicode           ; extended unicode support for various languages
       vc-gutter         ; vcs diff in the fringe
       vi-tilde-fringe   ; fringe tildes to mark beyond EOB
       window-select     ; visually switch windows
       workspaces        ; tab emulation, persistence & separate workspaces
       ;;zen               ; distraction-free coding or writing

       :input
       ;;chinese
       ;;japanese
       ;;layout

       :editor
       (evil +everywhere); come to the dark side, we have cookies
       file-templates    ; auto-snippets for empty files
       fold              ; (nigh) universal code folding
       (format +onsave)  ; automated prettiness
       ;;god               ; run Emacs commands without modifier keys
       ;;lispy             ; vim for lisp, for people who dont like vim
       multiple-cursors  ; editing in many places at once
       ;;objed             ; text object editing for the innocent
       ;;parinfer          ; turn lisp into python, sort of
       rotate-text       ; cycle region at point between text candidates
       snippets          ; my elves. They type so I don't have to
       word-wrap         ; soft wrapping with language-aware indent

       :emacs
       (dired  +icons)   ; making dired pretty [functional]
       electric          ; smarter, keyword-based electric-indent
       (ibuffer +icons)  ; interactive buffer management
       (undo +icons)     ; persistent, smarter undo for your inevitable mistakes
       vc                ; version-control and Emacs, sitting in a tree

       :term
       ;;eshell            ; the elisp shell that works everywhere
       ;;shell             ; simple shell REPL for Emacs
       ;;term              ; basic terminal emulator for Emacs
       ;;vterm             ; the best terminal emulation in Emacs

       :checkers
       syntax              ; tasing you for every semicolon you forget
       ;;spell             ; tasing you for misspelling mispelling
       ;;grammar           ; tasing grammar mistake every you make

       :tools
       ;;ansible
       ;;(debugger +lsp)   ; FIXME stepping through code, to help you add bugs
       ;;direnv
       docker
       editorconfig      ; let someone else argue about tabs vs spaces
       ;;ein               ; tame Jupyter notebooks with emacs
       (eval +overlay)     ; run code, run (also, repls)
       ;;gist              ; interacting with github gists
       (lookup              ; navigate your code and its documentation
        +dictionary
        +docsets)
       (lsp +peek)
       ;;macos             ; MacOS-specific commands
       magit             ; a git porcelain for Emacs
       ;;make              ; run make tasks from Emacs
       ;;pass              ; password manager for nerds
       ;;pdf               ; pdf enhancements
       ;;prodigy           ; FIXME managing external services & code builders
       ;;rgb               ; creating color strings
       ;;taskrunner        ; taskrunner for all your projects
       ;;terraform         ; infrastructure as code
       ;;tmux              ; an API for interacting with tmux
       ;;upload            ; map local to remote projects via ssh/ftp

       :os
       ;;arch
       ;;(:if IS-MAC macos)  ; improve compatibility with macOS
       ;;nixos
       ;;tty               ; improve the terminal Emacs experience

       :lang
       ;;agda
       ;;assembly
       ;;beancount
       ;;(cc +lsp)
       ;;crystal
       ;;clojure
       ;;(csharp +unity +lsp)
       ;;common-lisp       ; if you've seen one lisp, you've seen them all
       ;;coq
       data              ; config/data formats
       ;;(dart +lsp)
       ;;erlang
       ;;elixir
       ;;elm
       emacs-lisp        ; drown in parentheses
       ;;ess
       ;;faust
       ;;fsharp
       ;;go
       ;;haskell
       ;;hy
       ;;(java +meghanada)
       (javascript +lsp)        ; all(hope(abandon(ye(who(enter(here))))))
       (json +lsp)              ; At least it ain't XML
       ;;julia
       ;;latex
       ;;ledger
       ;;(lua +fennel)
       ;;markdown
       ;;nim
       ;;nix
       ;;ocaml
       org               ; organize your plain life in plain text
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
       ;;(scheme +guile)   ; a fully conniving family of lisps
       sh                ; she sells {ba,z,fi}sh shells on the C xor
       ;;sml
       ;;swift
       (web +lsp)               ; the tubes
       (yaml +lsp)              ; JSON, but readable

       :email
       ;;(mu4e +gmail)
       ;;notmuch
       ;;(wanderlust +gmail)

       :app
       ;;calendar
       ;;everywhere        ; *leave* Emacs!? You must be joking
       ;;irc
       ;;(rss +org)
       ;;ereader

       :config
       ;;literate
       (default +bindings +smartparens)

       ;;------------- Load Personal Modules --------------
       :private
       core
       appearance
       navigation
       notify
       chinese
       editor
       orgmode
       programming
      ;lang-python
      ;lsp-python
       service
       )

;(setq custom-file (expand-file-name "local/custom.el" doom-emacs-dir))
;(load custom-file 'no-error 'no-message)

(provide 'init)

;;; init.el ends here
