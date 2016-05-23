;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Themes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Load theme
(use-package zenburn-theme :ensure t)
(use-package monokai-theme
  :ensure t
  :config
  (progn
    (message "Monokai Theme - Loaded")
    (load-theme 'monokai t)
    (set-background-color "#121212")))

(defun my/change-theme()
  (interactive)
  (message "Theme Changed")
  )

(when window-system 
  (my/change-theme)
  )

(provide 'init-themes)
