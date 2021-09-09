;;; config.el -*- lexical-binding: t; -*-

;; ** loading
(load! "+ui")
(load! "+popup")

(use-package! all-the-icons-dired
  :hook (dired-mode . all-the-icons-dired-mode))

;; Discover key bindings and their meaning for the current Emacs major mode
(use-package! discover-my-major
  :defer t)

(use-package! awesome-tab
  :config
  (setq awesome-tab-cycle-scope 'tabs ; Navigate through visible tabs only.
        awesome-tab-display-sticky-function-name nil)
  (awesome-tab-mode t)
  (with-eval-after-load 'evil
    (define-key evil-normal-state-map (kbd ",tt") 'awesome-tab-switch-group)
    (define-key evil-normal-state-map (kbd ",ta") 'awesome-tab-select-beg-tab)
    (define-key evil-normal-state-map (kbd ",te") 'awesome-tab-select-end-tab)
    (define-key evil-normal-state-map (kbd ",t<") 'awesome-tab-move-current-tab-to-left)
    (define-key evil-normal-state-map (kbd ",t>") 'awesome-tab-move-current-tab-to-right)
    (define-key evil-normal-state-map (kbd ",tl") 'awesome-tab-forward)
    (define-key evil-normal-state-map (kbd ",th") 'awesome-tab-backward))
  )

(after! doom-modeline
  (remove-hook 'display-battery-mode-hook #'doom-modeline-override-battery-modeline)
  (remove-hook 'doom-modeline-mode-hook #'doom-modeline-override-battery-modeline)

  ; Define your custom doom-modeline
  (doom-modeline-def-modeline 'my-simple-line
    '(bar workspace-name window-number matches buffer-info remote-host buffer-position parrot selection-info)
    '(misc-info minor-modes input-method buffer-encoding major-mode process vcs checker))
  ; Set it to default using the doom-modeline-mode-hook:
  (defun setup-custom-doom-modeline ()
    (doom-modeline-set-modeline 'my-simple-line 'default))

  (add-hook 'doom-modeline-mode-hook 'setup-custom-doom-modeline)
  )

;; Dynamic Tiling Windows in Emacs with Edwina
(use-package! edwina
  :ensure t
  :custom
  (edwina-mfact 0.55)
  (edwina-narrow-threshold 115)
  :config
  (map! :leader
        (:prefix ("e" . "Edwina")
         :desc "Toggle Edwina" "e" #'edwina-mode
         :desc "Arrange" "r" #'edwina-arrange
         :desc "Next Window" "h" #'edwina-select-next-window
         :desc "Prev Window" "l" #'edwina-select-previous-window
         :desc "Swap Next" "L" #'edwina-swap-next-window
         :desc "Swap Prev" "H" #'edwina-swap-previous-window
         :desc "Dec MFact" "-" #'edwina-dec-mfact      ;; 主窗口缩窄
         :desc "Inc MFact" "=" #'edwina-inc-mfact      ;; 主窗口拉宽
         :desc "Dec Master" "_" #'edwina-dec-nmaster   ;; 减少主窗口的数量
         :desc "Inc Master" "+" #'edwina-inc-nmaster   ;; 增加主窗口的数量
         :desc "Del Window" "d" #'edwina-delete-window ;; 关闭窗口
         :desc "Zoom on Window" "z" #'edwina-zoom      ;; 交换「主窗口」和「副窗口」
         ))

  (edwina-mode 1))

;;
;; Keybindings
;;

(map! :leader
  (:desc "help" :prefix "h"
    :n "h" help-map
    :desc "Discover Emacs major mode"              :nv "z"  #'discover-my-major)
  )
