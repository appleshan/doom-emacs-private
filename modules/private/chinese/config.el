;;; config.el -*- lexical-binding: t; -*-

(defvar *large-buffer-threshold* 300000
  "Buffer whose size beyond it will have a different behavior for the efficiency")

;; Chinese calendar
(use-package! cal-china-x
  :defer 1
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

    ;; 在日历中显示假日
    (setq calendar-mark-holidays-flag t)
    ;; 按中国习惯，周一为每周第一天
    (setq calendar-week-start-day 1)

    (setq cal-china-x-important-holidays cal-china-x-chinese-holidays)
    (setq cal-china-x-general-holidays
          '(;;公历节日
            (holiday-fixed 1 1 "元旦")
            (holiday-fixed 3 8 "妇女节")     ; Women's Day
            (holiday-fixed 3 12 "植树节")    ; Arbor Day
            (holiday-fixed 5 1 "劳动节")     ; International Workers' Day
            (holiday-fixed 5 4 "青年节")     ; Chinese Youth Day
            (holiday-fixed 6 1 "儿童节")     ; Children's Day
            (holiday-fixed 9 10 "教师节")    ; Teachers' Day
            (holiday-fixed 10 1 "国庆节")    ; National Day
            (holiday-fixed 10 24 "程序员节") ; Programmers' Day
            ;;农历节日
            (holiday-chinese-new-year)
            (holiday-lunar 12 30 "除夕" 0)
            (holiday-lunar 1 1 "春节" 0)
            (holiday-lunar 1 2 "春节" 0)
            (holiday-lunar 1 3 "春节" 0)
            (holiday-lunar 1 15 "元宵节 (正月十五)") ; Lantern Festival
            (holiday-solar-term "清明" "清明节")
            (holiday-lunar 5 5 "端午节 (五月初五)" 0)
            (holiday-lunar 7 7 "七夕节 (七月初七)")
            (holiday-lunar 8 15 "中秋节 (八月十五)" 0)
            (holiday-lunar 9 9 "重阳节 (九月初九)" 0)
            (holiday-lunar 12 8 "腊八节" 0)
            (holiday-lunar 12 22 "冬至" 0)))

    (setq holiday-other-holidays
          '(;;其它节日
            (holiday-fixed 2 14 "情人节")
            (holiday-fixed 4 1 "愚人节")
            (holiday-fixed 4 22 "世界地球日") ; Earth Day
            (holiday-fixed 4 23 "世界读书日") ; World Book Day
            (holiday-fixed 12 25 "圣诞节")
            (holiday-float 5 0 2 "母亲节")
            (holiday-float 6 0 3 "父亲节")
            (holiday-float 11 4 4 "感恩节")
            (holiday-sexp '(if (or (zerop (% year 400))
                                   (and (% year 100) (zerop (% year 4))))
                               (list 9 12 year)
                             (list 9 13 year))
                         "俄罗斯的程序员节") ; World Programmers' Day
            ;;纪念日，已放入 memorial-day.org，此处注释备存
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

;; @See https://manateelazycat.github.io/emacs/2020/03/22/emacs-rime.html
(use-package! rime
  :defer t
  :bind
  (:map rime-active-mode-map
    ("<tab>" . 'rime-inline-ascii))
  (:map rime-mode-map
    ("C-`" . 'rime-send-keybinding)
    ("C-s-r" . 'rime-force-enable))
  :custom
  (default-input-method "rime")
  ;; @See https://github.com/DogLooksGood/emacs-rime/issues/133
  (rime-user-data-dir "~/.local/share/fcitx5/rime") ; 用户 RIME 配置文件所在地
  ;; 特定的场景下需要自动使用英文，若断言有一个非真，则自动进入英文模式
  (rime-disable-predicates
    '(;; rime-predicate-after-alphabet-char-p        ;在英文字符串之后（必须为以字母开头的英文字符串）
      ;; rime-predicate-after-ascii-char-p           ;任意英文字符后
      ;; rime-predicate-auto-english-p               ;在英文字符后面继续输入英文，空格切换中英文
      ;; rime-predicate-prog-in-code-p               ;在 prog-mode 和 conf-mode 中除了注释和引号内字符串之外的区域
      ;; rime-predicate-in-code-string-p             ;在代码的字符串中，不含注释的字符串。
      rime-predicate-evil-mode-p                  ;在 evil-mode 的非编辑状态下
      rime-predicate-ace-window-p                 ;激活 ace-window-mode
      rime-predicate-hydra-p                      ;如果激活了一个 hydra keymap
      ;; rime-predicate-current-input-punctuation-p  ;当要输入的是符号时
      rime-predicate-punctuation-after-space-cc-p ;当要在中文字符且有空格之后输入符号时
      rime-predicate-punctuation-after-ascii-p    ;当要在任意英文字符之后输入符号时
      rime-predicate-punctuation-line-begin-p     ;在行首要输入符号时
      ;; rime-predicate-space-after-ascii-p          ;在任意英文字符且有空格之后
      rime-predicate-space-after-cc-p             ;在中文字符且有空格之后
      rime-predicate-current-uppercase-letter-p   ;将要输入的为大写字母时
      ;; rime-predicate-tex-math-or-command-p        ;在 (La)TeX 数学环境中或者输入 (La)TeX 命令时
      ))
  (rime-translate-keybindings '("C-f" "C-b" "C-n" "C-p" "C-g"))
  (rime-title (char-to-string 12563))
  (rime-cursor "|")
  (rime-show-candidate 'posframe)
  (rime-posframe-properties
    (list :background-color "#333333"
      :foreground-color "#dcdccc"
      :font "WenQuanYi Micro Hei Mono-14"
      :internal-border-width 10))
  :custom-face
  (rime-code-face ((t (:foreground "#ee6363"))))          ;编码的颜色
  (rime-candidate-num-face ((t (:foreground "#ee6363")))) ;候选序号颜色
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

;; Evil search Chinese characters by pinyin
(use-package! evil-pinyin
  :init
  ;;(setq-default evil-pinyin-scheme 'simplified-xiaohe-all)
  ;;(setq-default evil-pinyin-with-search-rule 'always)
  :config
  ;;(evil-select-search-module 'evil-search-module 'evil-search)
  (global-evil-pinyin-mode))
