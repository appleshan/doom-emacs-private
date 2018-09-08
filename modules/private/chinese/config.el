;;; config.el -*- lexical-binding: t; -*-

(defvar *large-buffer-threshold* 300000
  "Buffer whose size beyond it will have a different behavior for the efficiency")

;; Chinese calendar
(def-package! cal-china-x
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

(def-package! pangu-spacing
  :config
  (progn
    (global-pangu-spacing-mode -1)
    ;; Always insert `real' space in org-mode.
    (add-hook 'org-mode-hook
              '(lambda ()
                 (set (make-local-variable 'pangu-spacing-real-insert-separtor) t)))

    (defun enable-pangu-spacing-when-buffer-not-large ()
      "when buffer is not large, turn on it"
      (when (< (buffer-size) *large-buffer-threshold*)
        (pangu-spacing-mode 1)))

    (dolist (i '(org-mode-hook prog-mode-hook text-mode-hook))
      (add-hook i 'enable-pangu-spacing-when-buffer-not-large))))

(def-package! pyim
  :init
  (setq pyim-directory (expand-file-name ".local/pyim/" user-emacs-directory)
        pyim-dcache-directory (expand-file-name "dcache/" pyim-directory)
        default-input-method "pyim")
  :config
  (progn
    ;; 设置 pyim 探针设置，这是 pyim 高级功能设置，可以实现 *无痛* 中英文切换.
    ;; 我自己使用的中英文动态切换规则是：
    ;; 1. 光标只有在注释里面时，才可以输入中文。
    ;; 2. 光标前是汉字字符时，才能输入中文。
    ;; 3. 使用 M-j 快捷键，强制将光标前的拼音字符串转换为中文。
    ;(setq-default pyim-english-input-switch-functions
    ;              '(pyim-probe-dynamic-english
    ;                pyim-probe-isearch-mode
    ;                pyim-probe-program-mode
    ;                pyim-probe-org-structure-template))

    ;(setq-default pyim-punctuation-half-width-functions
    ;              '(pyim-probe-punctuation-line-beginning
    ;                pyim-probe-punctuation-after-punctuation))

    (if (version< emacs-version "26")
      (setq pyim-page-tooltip t) ; emacs 25
      ; 使用 emacs 26 的 child-frame
      ; @see https://emacs-china.org/t/topic/4451
      (setq pyim-page-tooltip 'child-frame))

    ;; 选词框显示5个候选词
    (setq pyim-page-length 9)

    ;; 让 Emacs 启动时自动加载 pyim 词库
    ;(add-hook 'emacs-startup-hook
    ;          #'(lambda () (pyim-restart-1 t)))

    ;; 禁用 dabberv 中文补全
    (setq pyim-company-complete-chinese-enable nil)
    ))

(def-package! pyim-basedict
  :config
  (with-eval-after-load 'pyim
    (pyim-basedict-enable)))

;(def-package! pyim-greatdict
;  :config
;  (with-eval-after-load 'pyim
;    (pyim-greatdict-enable)))
