;;; config.el -*- lexical-binding: t; -*-

;; Files and directory variables
(setq org-directory (expand-file-name "~/projects-private/org-notes/")
      org-gtd-dir (expand-file-name "~/projects-private/org-gtd/"))

;; lang/org
(setq org-agenda-files (list org-gtd-dir)

      ;; The standard unicode characters are usually misaligned depending on the
      ;; font. This bugs me. Personally, markdown #-marks for headlines are more
      ;; elegant.
      org-bullets-bullet-list '("#"))

;;{{ 更好看的符号列表标记
;; @see https://github.com/lujun9972/emacs-document/blob/master/org-mode/%E5%B0%86org%E7%9C%8B%E6%88%90%E6%96%87%E5%AD%97%E5%A4%84%E7%90%86%E5%99%A8.org
;; 这段代码将所有行中匹配指定正则表达式的内容都显示为一个Unicode的圆形符号,
;; 该段正则的意思是“以 1 个或多个破折号开头,紧接着是一个空格”.
;; 用星号和破折号来作为符号列表的标记挺好的, 但是使用一个真正的圆形符号来作标示也不错:
(font-lock-add-keywords
 'org-mode
 '(("^\\([-]\\) "
    (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))
;;}}
