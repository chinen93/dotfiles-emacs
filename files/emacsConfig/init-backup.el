;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Backup
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Set directory to hold backup files
(setq backup-directory-alist '(("." . "~/.emacs.d/backups")))

;; Set transform to apply to buffer file name before making auto-save
;; file name.
(setq auto-save-file-name-transforms
      '((".*" "~/.emacs.d/auto-save-list/" t)))

;; Backup by copying, delete old verions and backup version controlled
;; files.
(setq backup-by-copying t)
(setq delete-old-versions t)
(setq version-control t)

(provide 'init-backup)
