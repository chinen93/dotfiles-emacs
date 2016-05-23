;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Helm Swoop Package
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package helm-swoop
  :ensure t
  :init
  (progn
    (message "Helm Swoop - Loaded")
    
    ;; Make Swoop faster
    (setq helm-swoop-speed-or-color t)
    
    ;; make swoop in actual window
    (setq helm-swoop-split-with-multiple-windows t)
    
    ;; Bind key
    (global-set-key (kbd "C-f") 'helm-swoop)))


(provide 'init-helmSwoop)
