;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Macro
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package elmacro
  :defer 2
  :ensure t
  :config
  (progn
    (message "Elmacro - Loaded")
    (elmacro-mode)))

(provide 'init-macro)
