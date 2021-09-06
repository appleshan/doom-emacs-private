;;; init.el -*- lexical-binding: t; -*-

(defhydra +hydra/dired (:color blue :hint nil)
  "
Mark              Operate         Misc              Navigate
----              -------         ----              --------
_fd_: flag del    _C_: copy       _+_: mkdir        _<up>_: up directory
_f#_: autosave    _R_: rename     _o_: open other
_f~_: backups     _D_: delete
_f&_: garbage     _F_: open marks
_fe_: extension
----
_m_: mark         _T_: touch
_/_: directories  _M_: chmod
_@_: symlinks     _G_: chgrp
_O_: omitted      _O_: chown
----
_U_: unmark all   _A_: find regx
_t_: toggle marks _Q_: find/rep
"
  ;; marking
  ("t" dired-toggle-marks)
  ("m" dired-mark :exit nil)
  ("u" dired-unmark :exit nil)
  ("fd" dired-flag-file-deletion)
  ("f#" dired-flag-auto-save-files)
  ("f~" dired-flag-backup-files)
  ("f&" dired-flag-garbage-files)
  ("fe" dired-flag-extension)
  ("/" dired-mark-directories)
  ("@" dired-mark-symlinks)
  ("." dired-mark-extension)
  ("O" dired-mark-omitted)
  ("U" dired-unmark-all-marks)

  ("C" dired-do-copy)
  ("R" dired-do-rename)
  ("D" dired-do-delete :exit nil)
  ("F" dired-do-find-marked-files)
  ("!" dired-do-shell-command)
  ("&" dired-do-async-shell-command)

  ("T" dired-do-touch)
  ("M" dired-do-chmod)
  ("G" dired-do-chgrp)
  ("O" dired-do-chown)

  ("A" dired-do-find-regexp)
  ("Q" dired-do-find-regexp-and-replace)

  ("+" dired-create-directory)
  ("o" dired-find-file-other-window)

  ("<up>" dired-up-directory)
  )
