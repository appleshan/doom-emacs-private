;;; config.el -*- lexical-binding: t; -*-

(def-package! pyim
  :init
  (setq pyim-directory (expand-file-name "pyim/" user-emacs-directory)
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
    (add-hook 'emacs-startup-hook
              #'(lambda () (pyim-restart-1 t)))

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
