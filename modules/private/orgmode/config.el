;;; config.el -*- lexical-binding: t; -*-

(after! org

  ;; 防止不小心编辑了省略部分的内容
  (setq org-catch-invisible-edits 'smart)

  ;; 不显示headline之间的空白行
  (setq org-cycle-separator-lines 0)

  ;; 比较喜欢使用 ⤵ 指待隐藏内容
  (setq org-ellipsis "⤵")

  ;;{{ 仅仅显示斜体字就好
  ;; /org italic/ 看起来就好像是斜体字被正则表达式的分隔符所包围了一样. 隐藏这些标记很简单
  (setq org-hide-emphasis-markers t)
  ;; 记住,这些斜杠字符(用于标示粗体的星号等其他字符也是一样)依然存在的,只是没有显示出来而已.
  ;; 想要修改这些标记也很简单,只要在之上按退格键就行.
  ;;}}

  ;;{{ 更好看的符号列表标记
  ;; 这段代码将所有行中匹配指定正则表达式的内容都显示为一个Unicode的圆形符号,
  ;; 该段正则的意思是“以 1 个或多个破折号开头,紧接着是一个空格”.
  ;; 用星号和破折号来作为符号列表的标记挺好的, 但是使用一个真正的圆形符号来作标示也不错:
  (font-lock-add-keywords
   'org-mode
   '(("^ \\([-+]\\) "
      (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))
  ;;}}

  ;; Add new easy templates
  (setq org-structure-template-alist
        (append
          '(("sb" "#+BEGIN_SRC bash\n?\n#+END_SRC")
            ("se" "#+BEGIN_SRC emacs-lisp\n?\n#+END_SRC")
            ("sp" "#+BEGIN_SRC python\n?\n#+END_SRC")
            ("su" "#+BEGIN_SRC plantuml\n?\n#+END_SRC")
            )
          org-structure-template-alist))

  ;;;;;;;;;;;;;;;;;;;;
  ;; TODO 状态触发器
  ;;;;;;;;;;;;;;;;;;;;

  ;; 当 TODO 状态发生更改时,自动添加/删除特定的 TAG ,这样方便 agenda view 中过滤任务:
  ;; org-todo-state-tags-triggers 的格式为:
  ;; `(state-change (tag . flag) …….)’
  ;; 这里 state-change 可以是一个表示 todo 状态的字符串,或者是符号 ’todo 或 ’done ,
  ;; 分别表示所有表示未完成任务的和以完成任务的 todo state
  (setq org-todo-state-tags-triggers
        (quote (("CANCELLED" ("CANCELLED" . t))
                ("WAITING" ("WAITING" . t))
                ("HOLD" ("WAITING") ("HOLD" . t))
                (done ("WAITING") ("HOLD"))
                ("TODO" ("WAITING") ("CANCELLED") ("HOLD"))
                ("NEXT" ("WAITING") ("CANCELLED") ("HOLD"))
                ("DONE" ("WAITING") ("CANCELLED") ("HOLD")))))
  ;; note:
  ;; * Moving a task to CANCELLED adds a CANCELLED tag
  ;; * Moving a task to WAITING adds a WAITING tag
  ;; * Moving a task to HOLD adds WAITING and HOLD tags
  ;; * Moving a task to a done state removes WAITING and HOLD tags
  ;; * Moving a task to TODO removes WAITING, CANCELLED, and HOLD tags
  ;; * Moving a task to NEXT removes WAITING, CANCELLED, and HOLD tags
  ;; * Moving a task to DONE removes WAITING, CANCELLED, and HOLD tags
  )

;; org-crypt 加密
;; @see https://coldnew.github.io/4bb1df06/
;; 使用`org-crypt’库,可以自动将带”:secret:” tag 的 headline ,在写入时加密存储.
;; 该功能对于想要将密码等隐私消息存入org文件带来便利.
(after! org-crypt
  ;; 設定要加密的 tag 標籤為 secret
  (setq org-crypt-tag-matcher "secret")

  ;; 设置 secret 标签不参与继承,避免造成重複加密
  ;; (但是子項目還是會被加密喔)
  (setq org-tags-exclude-from-inheritance (quote ("secret")))

  ;; GPG key to use for encryption
  ;; Either the Key ID or set to nil to use symmetric encryption.
  (setq org-crypt-key nil)

  ;; 要想解密 headline,则需要在光标定位到加密内容处,然后执行`M-x org-decrypt-entry’
  ;; 默认情况下, Emacs 会定时自动保持在编辑的文件,
  ;; 若此时在编辑的文件为密码文件且内容已经被解密,则可能存在将解密后的文本保存
  ;; 到磁盘上, 从而造成敏感信息泄露的情况,因此一般我们在编辑 crypt 文件时,取消
  ;; 自动保存功能
  (setq org-crypt-disable-auto-save t))

(after! org-list
  ;; 允许使用字母作为list bullet
  (setq org-list-allow-alphabetical t)

  ;; 自动切换list bullet
  ;; 若每个层级的list都使用同样的list bullet,则可能造成难于区分哪个list entry
  ;; 是属于哪个层级的. org-mode提供了当改变list层级时自动改变list bullet的机制
  (setq org-list-demote-modify-bullet '(("+" . "-")
                                        ("*" . "-")
                                        ("1." . "-")
                                        ("1)" . "-")
                                        ("A)" . "-")
                                        ("B)" . "-")
                                        ("a)" . "-")
                                        ("b)" . "-")
                                        ("A." . "-")
                                        ("B." . "-")
                                        ("a." . "-")
                                        ("b." . "-"))))

(after! org-agenda
  ;; 设置agenda的数据来源
  ;; org-agenda-files中的元素还可以是目录，这时目录下的所有匹配
  ;; `org-agenda-file-regexp’的文件都自动加入 agenda .
  (setq org-agenda-files
    (list
        (concat org-gtd-dir "inbox.org")
        (concat org-gtd-dir "project.org")
        (concat org-gtd-dir "task.org")
        (concat org-gtd-dir "finished.org")
        (concat org-gtd-dir "trash.org")
        (concat org-gtd-dir "memorial-day.org")
        ))
  )

(after! org-faces
  (setq org-todo-keyword-faces
        (quote (("TODO" :foreground "red" :weight bold)
                ("STARTED" :foreground "blue" :weight bold)
                ("WAITING" :foreground "orange" :weight bold)
                ("APPT" :foreground "magenta" :weight bold)
                ("DONE" :foreground "forest green" :weight bold)
                ("CANCELLED" :foreground "forest green" :weight bold)
                ("DEFERRED" :foreground "forest green" :weight bold)
                ("MEETING" :foreground "forest green" :weight bold)
                ("PHONE" :foreground "forest green" :weight bold))))

  )

(use-package! org-super-agenda
  :config (org-super-agenda-mode))

(use-package! secretaria
  :after (alert f s)
  :config
  ;; use this for getting a reminder every 30 minutes of those tasks scheduled
  ;; for today and which have no time of day defined.
  (add-hook 'after-init-hook #'secretaria-unknown-time-always-remind-me))

