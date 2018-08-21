;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Nyan Cat Package
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package nyan-mode
  :ensure t
  :config
  (progn
    (message "Nyan Mode - Loaded")

    ;; Max length of the nyan rainbow trail
    (setq nyan-bar-length 10)

    ;; start nyan mode
    (nyan-mode 1)))

(provide 'init-nyanCat)
