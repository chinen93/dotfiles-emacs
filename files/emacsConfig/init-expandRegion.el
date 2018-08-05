;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Expand Region Package
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package expand-region
  :defer 2
  :ensure t
  :config
  (progn
    (message "Expand Region - Loaded")
    ;; Bind key to command
    (global-set-key (kbd "C-=") 'er/expand-region)))

(provide 'init-expandRegion)
