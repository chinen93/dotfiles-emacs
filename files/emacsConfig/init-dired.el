;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Dired Package
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; -a : show all entries even those "hidden".
;; -l : use a long listing format.
;; -H : follow symbolic links.
;; --group-directories-first : directory before files.
(setq dired-listing-switches "-alH --group-directories-first")

(provide 'init-dired)
