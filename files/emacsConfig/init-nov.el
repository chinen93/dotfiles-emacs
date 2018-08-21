;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Nov Package
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package nov
  :defer 2
  :ensure t
  :config
  (progn
    (message "Nov Mode - Loaded")

    (setq nov-text-width most-positive-fixnum)
    (add-hook 'nov-mode-hook 'visual-line-mode)

    (setq nov-text-width 80)

    (defun my-nov-font-setup ()
      (face-remap-add-relative
       'variable-pitch
       :family "Liberation Serif"
       :height 1.0))

    (add-hook 'nov-mode-hook 'my-nov-font-setup)

    (add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode))

    ))

(provide 'init-nov)
