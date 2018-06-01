;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Dired Package
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; -a : show all entries even those "hidden".
;; -l : use a long listing format.
;; -H : follow symbolic links.
;; --group-directories-first : directory before files.
(setq dired-listing-switches "-Alh --group-directories-first --sort=extension")

(defun xah-dired-mode-setup ()
  "to be run as hook for `dired-mode'."
  (dired-hide-details-mode 1))
(add-hook 'dired-mode-hook 'xah-dired-mode-setup)

(provide 'init-dired)
