;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Backup
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Set directory to hold backup files
(setq backup-directory-alist '(("." . "~/.emacs.d/backups")))
(setq auto-save-file-name-transforms
      '((".*" "~/.emacs.d/auto-save-list/" t)))

(setq backup-by-copying t)
(setq delete-old-versions t)
(setq version-control t)

(provide 'init-backup)
