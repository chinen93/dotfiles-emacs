;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Diminish
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package diminish
  :defer 3
  :ensure t
  :config 
  (progn
    (diminish 'ivy-mode)
    (diminish 'company-mode)
    (diminish 'elmacro-mode)
    (diminish 'yas-global-mode)
    (diminish 'yas-minor-mode)
    (diminish 'flycheck-mode " FC")
    (diminish 'auto-revert-mode)

    (diminish 'helm)))

(provide 'init-diminish)
