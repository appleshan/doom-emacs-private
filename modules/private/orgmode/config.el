;;; config.el -*- lexical-binding: t; -*-

(after! org
  ;; 更好看的标题: 让headers变得更大而不是换个颜色.
  (let* ((variable-tuple (cond ((x-list-fonts "Source Sans Pro") '(:font "Source Sans Pro"))
                               ((x-list-fonts "Lucida Grande")   '(:font "Lucida Grande"))
                               ((x-list-fonts "Verdana")         '(:font "Verdana"))
                               ((x-family-fonts "Sans Serif")    '(:family "Sans Serif"))
                               (nil (warn "Cannot find a Sans Serif Font.  Install Source Sans Pro."))))
         (base-font-color     (face-foreground 'default nil 'default))
         (headline           `(:inherit default :weight bold :foreground ,base-font-color)))

    (custom-theme-set-faces 'user
                            `(org-level-8 ((t (,@headline ,@variable-tuple))))
                            `(org-level-7 ((t (,@headline ,@variable-tuple))))
                            `(org-level-6 ((t (,@headline ,@variable-tuple))))
                            `(org-level-5 ((t (,@headline ,@variable-tuple))))
                            `(org-level-4 ((t (,@headline ,@variable-tuple :height 1.1))))
                            `(org-level-3 ((t (,@headline ,@variable-tuple :height 1.25))))
                            `(org-level-2 ((t (,@headline ,@variable-tuple :height 1.5))))
                            `(org-level-1 ((t (,@headline ,@variable-tuple :height 1.75))))
                            `(org-document-title ((t (,@headline ,@variable-tuple :height 1.5 :underline nil))))))

  ;; 防止不小心编辑了省略部分的内容
  (setq org-catch-invisible-edits 'smart)

  ;; 不显示headline之间的空白行
  (setq org-cycle-separator-lines 0)

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
