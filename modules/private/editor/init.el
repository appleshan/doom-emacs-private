;;; init.el -*- lexical-binding: t; -*-

(setq-default fill-column 79)

(setq-default whitespace-line-column 80)
(setq-default whitespace-style '(face
                                 indentation
                                 tabs tab-mark
                                 spaces space-mark
                                 newline newline-mark
                                 trailing lines-tail))

;; 把空格和回车符号可视化
;(add-hook   'find-file-hook #'whitespace-mode)

(add-hook 'before-save-hook #'whitespace-cleanup)

;; 绑定扩展名到特定的模式
(dolist (elt-cons '((".*rc\\'" . conf-mode)
                    ("\\.myclirc\\'" . conf-mode)
                    (".xprofile'" . conf-mode)
                    ("torrc'" . conf-mode)
                    ("mirrorlist" . conf-mode)
                    ("\\.lrc\\'" . emms-lyrics-mode)
                    ("\\.org\\'" . org-mode)
                    ("\\.cron\\(tab\\)?\\'" . crontab-mode)
                    ("cron\\(tab\\)?\\." . crontab-mode)))
  (add-to-list 'auto-mode-alist elt-cons))

;; {{ scroll functions
(defun +editor|hold-line-scroll-up()
  "Scroll the page with the cursor in the same line"
  (interactive)
  ;; move the cursor also
  (let ((tmp (current-column)))
    (scroll-up 1)
    (line-move-to-column tmp)
    (forward-line 1)))

(defun +editor|hold-line-scroll-down()
  "Scroll the page with the cursor in the same line"
  (interactive)
  ;; move the cursor also
  (let ((tmp (current-column)))
    (scroll-down 1)
    (line-move-to-column tmp)
    (forward-line -1)))

(global-set-key [(control down)] '+editor|hold-line-scroll-up)
(global-set-key [(control up)]   '+editor|hold-line-scroll-down)
;; }}

;; 逗号后面自动加空格
(global-set-key (kbd ",") #'(lambda () (interactive) (insert ", ")))

;; Display next page at the other window
;; @See https://github.com/condy0919/emacs-newbie/blob/master/introduction-to-builtin-modes.md#follow-mode
(global-set-key (kbd "C-c M-f") 'follow-mode)

(defun +editor|open-readme-in-git-root-directory ()
  (interactive)
  (let (filename
        (root-dir
          (locate-dominating-file
            (file-name-as-directory
              (file-name-directory buffer-file-name)) ".git"))
        )
    ;; (message "root-dir=%s" root-dir)
    (and root-dir (file-name-as-directory root-dir))
    (setq filename (concat root-dir "README.org"))
    (if (not (file-exists-p filename))
        (setq filename (concat root-dir "README.md"))
      )
    ;; (message "filename=%s" filename)
    (if (file-exists-p filename)
        (switch-to-buffer (find-file-noselect filename nil nil))
      (message "NO README.org or README.md found!"))
    ))

;; {{ This snippet shows line numbers temporarily just
;; when you're going to a line number with goto-line.
(defun goto-line-with-feedback ()
  "Show line numbers temporarily, while prompting for the line number input"
  (interactive)
  (unwind-protect
      (progn
        (linum-mode 1)
        (goto-line (read-number "Goto line: ")))
    (linum-mode -1)))

;; Notice the nice remap-trick in the key binding.
;; It will remap all key bindings from goto-line to goto-line-with-feedback.
;; Neat!
(global-set-key [remap goto-line] 'goto-line-with-feedback)
;; }}
