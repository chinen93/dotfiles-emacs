;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Helm Package
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package helm
  :ensure t
  :diminish helm-mode
  :init
  (progn
    (require 'helm-config)
    (message "Helm - Loaded")
    
    ;; set max number of candidates
    (setq helm-candidate-number-limit 100)
    
    ;; From https://gist.github.com/antifuchs/9238468
    ;; update fast sources immediately (doesn't).
    (setq helm-idle-delay 0.0) 

    ;; this actually updates things
    (setq helm-input-idle-delay 0.01) 

    ;; reeeelatively quickly.
    (setq helm-yas-display-key-on-candidate t)
    (setq helm-quick-update t)

    ;; FIXME
    (setq helm-M-x-requires-pattern nil)
    (setq helm-split-window-in-side-p t)
    (setq helm-ff-skip-boring-files t)
    
    ;; start mode
    (helm-mode)

    ;; key binding
    (global-set-key (kbd "C-c h") 'helm-mini)
    (global-set-key (kbd "C-h a") 'helm-apropos)
    (global-set-key (kbd "C-x b") 'helm-buffers-list)
    (global-set-key (kbd "C-x C-b") 'helm-buffers-list)
    (global-set-key (kbd "C-x C-f") 'helm-find-files)
    (global-set-key (kbd "M-y") 'helm-show-kill-ring)
    (global-set-key (kbd "M-x") 'helm-M-x)
    
    ;; uses my key prefix
    (global-set-key (kbd "M-v M-f") 'helm-find-files)
    (global-set-key (kbd "M-v M-p") 'helm-bookmarks)
    ))

(provide 'init-helm)
