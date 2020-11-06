;;; init.el -*- lexical-binding: t; -*-

;; set-envs
(let ((path (shell-command-to-string ". ~/.oh-my-zsh/custom/path.zsh; echo -n $PATH")))
  (dolist (item (split-string-and-unquote path ":"))
    (add-to-list 'exec-path item))
  (setenv "PATH" path)
  (setenv "HOME" "/home/alecshan")
  (setenv "JAR_PATH" "/home/alecshan/bin/java" ))
