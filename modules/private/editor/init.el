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
(global-set-key [(control up)] '+editor|hold-line-scroll-down)
;; }}

;; {{ Move Current Line Up or Down
;; @see http://emacsredux.com/blog/2013/04/02/move-current-line-up-or-down/
(defun +editor|move-line-up ()
  "Move up the current line."
  (interactive)
  (transpose-lines 1)
  (forward-line -2)
  (indent-according-to-mode))

(defun +editor|move-line-down ()
  "Move down the current line."
  (interactive)
  (forward-line 1)
  (transpose-lines 1)
  (forward-line -1)
  (indent-according-to-mode))

(global-set-key (kbd "M-<up>") '+editor|move-line-up)
(global-set-key (kbd "M-<down>") '+editor|move-line-down)
;; }}

;; 逗号后面自动加空格
(global-set-key (kbd ",") #'(lambda () (interactive) (insert ", ")))

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

;;{{ 特殊字符与数字键交换
(defvar *unshifted-special-chars-layout*
  '(("1" "!") ; from -> to
    ("2" "@")
    ("3" "#")
    ("4" "$")
    ("5" "%")
    ("6" "^")
    ("7" "&")
    ("8" "*")
    ("9" "(")
    ("0" ")")
    ("!" "1")
    ("@" "2")
    ("#" "3")
    ("$" "4")
    ("%" "5")
    ("^" "6")
    ("&" "7")
    ("*" "8")
    ("(" "9")
    (")" "0")))

(defun mb-str-to-unibyte-char (s)
  "Translate first multibyte char in s to internal unibyte representation."
  (multibyte-char-to-unibyte (string-to-char s)))

(defun remap-keyboard (mapping)
  "Setup keyboard translate table using a list of pairwise key-mappings."
  (mapcar
   (lambda (mb-string-pair)
     (apply #'keyboard-translate
        (mapcar #'mb-str-to-unibyte-char mb-string-pair)))
   mapping))

(remap-keyboard *unshifted-special-chars-layout*)
;;}}
