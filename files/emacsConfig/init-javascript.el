;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Javascript Configuration
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package js2-mode
  :defer t
  :ensure t
  :config
  (progn
    ;; configs here
    (message "Javascript Configuration - Loaded")

    ;; number of spaces when identing
    (setq js2-basic-offset 2)

    ;; load this mode when loading .js files

    (add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
    (add-to-list 'auto-mode-alist '("\\.json\\'" . js2-mode))))

(provide 'init-javascript)
