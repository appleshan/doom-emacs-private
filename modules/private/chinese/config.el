;;; config.el -*- lexical-binding: t; -*-

(defvar *large-buffer-threshold* 300000
  "Buffer whose size beyond it will have a different behavior for the efficiency")

;; Chinese calendar
(use-package! cal-china-x
  ;:defer t
  ;:init (require 'cal-china-x)
  :commands cal-china-x-setup
  :init (add-hook 'calendar-load-hook #'cal-china-x-setup)
  :config
  (progn
    ;; `S' can show the time of sunrise and sunset on Calendar
    (setq calendar-location-name "Guangzhou"
          calendar-latitude 23.16
          calendar-longitude 113.23)

    (setq mark-holidays-in-calendar t)

    (setq christian-holidays nil) ;; 不显示基督教的节日
    (setq hebrew-holidays nil)    ;; 不显示希伯来人的节日
    (setq islamic-holidays nil)   ;; 不显示伊斯兰教的节日

    ;; Holidays
    (setq calendar-mark-holidays-flag t)

    (setq cal-china-x-important-holidays cal-china-x-chinese-holidays)
    (setq cal-china-x-general-holidays
          '(;; 农历节日
            (holiday-lunar 1 1 "春节" 0)
            (holiday-lunar 1 2 "春节" 0)
            (holiday-lunar 1 3 "春节" 0)
            (holiday-lunar 1 15 "元宵节 (正月十五)")
            (holiday-solar-term "清明" "清明节")
            (holiday-lunar 5 5 "端午节 (五月初五)" 0)
            (holiday-lunar 7 7 "七夕情人节 (七月初七)")
            (holiday-lunar 8 15 "中秋节 (八月十五)" 0)
            (holiday-lunar 9 9 "重阳节 (九月初九)" 0)
            (holiday-lunar 12 8 "腊八节" 0)
            (holiday-lunar 12 22 "冬至" 0)
            (holiday-fixed 3 8 "妇女节")
            (holiday-fixed 3 12 "植树节")
            (holiday-fixed 5 4 "青年节")
            (holiday-fixed 6 1 "儿童节")
            (holiday-fixed 9 10 "教师节")))
    (setq holiday-other-holidays
          '((holiday-fixed 2 14 "情人节")
            (holiday-fixed 4 1 "愚人节")
            (holiday-fixed 12 25 "圣诞节")
            (holiday-float 5 0 2 "母亲节")
            (holiday-float 6 0 3 "父亲节")
            (holiday-float 11 4 4 "感恩节")
            ;; 生日 -- 家人,朋友
            (holiday-fixed 11 18 "女儿生日")
            (holiday-lunar 4 25 "老婆生日" 0)
            (holiday-lunar 7 10 "我的生日" 0)
            ))
    ;; 只显示我定制的节日
    (setq calendar-holidays
          (append cal-china-x-important-holidays
                  cal-china-x-general-holidays
                  holiday-other-holidays))
    ))

(use-package! pangu-spacing
  :config
  (progn
    (global-pangu-spacing-mode -1)

    ;; Always insert `real' space in org-mode.
    (setq-hook! 'org-mode-hook pangu-spacing-real-insert-separtor t)

    (defun enable-pangu-spacing-when-buffer-not-large ()
      "when buffer is not large, turn on it"
      (when (< (buffer-size) *large-buffer-threshold*)
        (pangu-spacing-mode 1)))

    (dolist (i '(org-mode-hook prog-mode-hook text-mode-hook))
      (add-hook i 'enable-pangu-spacing-when-buffer-not-large))))

(use-package! rime
  :commands (toggle-input-method)
  :bind
  (:map rime-active-mode-map
        ("<tab>" . 'rime-inline-ascii))
  (:map rime-mode-map
        ("C-`" . 'rime-send-keybinding)
        ("C-s-r" . 'rime-force-enable))
  :custom
  (rime-user-data-dir "~/.config/fcitx/rime/")
  (rime-disable-predicates '(helm--alive-p                 ; 让 Helm 不继承输入法的状态
                             rime-predicate-evil-mode-p
                             rime-predicate-prog-in-code-p ; 在 prog-mode 和 conf-mode 中除了注释和引号内字符串之外的区域
                             rime-predicate-auto-english-p ; 在英文字符后面继续输入英文，空格切换中英文
                             ))
  (rime-translate-keybindings '("C-f" "C-b" "C-n" "C-p" "C-g"))
  (default-input-method "rime")
  (rime-title (char-to-string 12563))
  (rime-cursor "|")
  (rime-show-candidate 'posframe)
  (rime-posframe-properties
    (list :background-color "#333333"
          :foreground-color "#dcdccc"
          :font "WenQuanYi Micro Hei Mono-14"
          :internal-border-width 10))
  :config
  (defun +rime--posframe-display-content-a (args)
    "给 `rime--posframe-display-content' 传入的字符串加一个全角空
格，以解决 `posframe' 偶尔吃字的问题。"
    (cl-destructuring-bind (content) args
      (let ((newresult (if (string-blank-p content)
                           content
                         (concat content "　"))))
        (list newresult))))

  (if (fboundp 'rime--posframe-display-content)
      (advice-add 'rime--posframe-display-content
                  :filter-args
                  #'+rime--posframe-display-content-a)
    (error "Function `rime--posframe-display-content' is not available."))
  )
