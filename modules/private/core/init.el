;;; init.el -*- lexical-binding: t; -*-

;; for linux
(when IS-LINUX
  (let ((mypaths
          '("/opt/java/jdk1.8.0_162/bin"
            "/opt/java/apache-maven-3.5.2/bin"
            "/home/appleshan/.local/bin"
            "/home/appleshan/bin"
            "/usr/local/sbin"
            "/usr/local/bin"
            "/usr/bin"
            "/usr/bin/site_perl"
            "/usr/bin/vendor_perl"
            "/usr/bin/core_perl"
           )))
    (setq exec-path (append mypaths (list "." exec-directory)))

    (setenv "PATH" (mapconcat 'identity mypaths ":") )
    (setenv "HOME" "/home/appleshan")
    (setenv "JAR_PATH" "/opt/java" )
    )
  )
