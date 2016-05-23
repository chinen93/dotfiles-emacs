;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Flycheck
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package flycheck
  :ensure t
  :config
  (progn
    (message "Flycheck - Loaded")

    ;; Flycheck gets to be a bit much when warning about checkdoc issues.
    (setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc))

    (add-hook 'prog-mode-hook 'flycheck-mode)
    ))

(provide 'init-flycheck)
