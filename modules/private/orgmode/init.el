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

;; {{ Move single cells using C-M-up C-M-down C-M-left C-M-right
(add-hook 'org-mode-hook
 '(lambda ()
    (local-set-key [C-M-up] (quote +orgmode|org-table-move-single-cell-up))
    (local-set-key [C-M-down] (quote +orgmode|org-table-move-single-cell-down))
    (local-set-key [C-M-left] (quote +orgmode|org-table-move-single-cell-left))
    (local-set-key [C-M-right] (quote +orgmode|org-table-move-single-cell-right))))

(defun +orgmode|org-table-swap-cells (i1 j1 i2 j2)
  "Swap two cells"
  (let ((c1 (org-table-get i1 j1))
  (c2 (org-table-get i2 j2)))
    (org-table-put i1 j1 c2)
    (org-table-put i2 j2 c1)
    (org-table-align)))

(defun +orgmode|org-table-move-single-cell (direction)
  "Move the current cell in a cardinal direction according to the
parameter symbol: 'up 'down 'left 'right. Swaps contents of
adjacent cell with current one."
  (unless (org-at-table-p)
    (error "No table at point"))
  (let ((di 0) (dj 0))
    (cond ((equal direction 'up) (setq di -1))
    ((equal direction 'down) (setq di +1))
    ((equal direction 'left) (setq dj -1))
    ((equal direction 'right) (setq dj +1))
    (t (error "Not a valid direction, must be up down left right")))
    (let* ((i1 (org-table-current-line))
     (j1 (org-table-current-column))
     (i2 (+ i1 di))
     (j2 (+ j1 dj)))
      (+orgmode|org-table-swap-cells i1 j1 i2 j2)
      (org-table-goto-line i2)
      (org-table-goto-column j2))))

(defun +orgmode|org-table-move-single-cell-up ()
  "Move a single cell up in a table; swap with anything in target cell"
  (interactive)
  (+orgmode|org-table-move-single-cell 'up))

(defun +orgmode|org-table-move-single-cell-down ()
  "Move a single cell down in a table; swap with anything in target cell"
  (interactive)
  (+orgmode|org-table-move-single-cell 'down))

(defun +orgmode|org-table-move-single-cell-left ()
  "Move a single cell left in a table; swap with anything in target cell"
  (interactive)
  (+orgmode|org-table-move-single-cell 'left))

(defun +orgmode|org-table-move-single-cell-right ()
  "Move a single cell right in a table; swap with anything in target cell"
  (interactive)
  (+orgmode|org-table-move-single-cell 'right))
;; }}
