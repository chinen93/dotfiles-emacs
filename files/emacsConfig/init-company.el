;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Company package
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package company
  :ensure t
  :config
  (progn
    (setq company-idle-delay 0)
    (setq company-echo-delay 0)
    (setq company-dabbrev-downcase nil)

    ;; minimimum size to start to search for match
    (setq company-minimum-prefix-length 2)
    (setq company-selection-wrap-around t)

    ;; sort matches by occurrence and backend importance
    (setq company-transformers '(company-sort-by-occurrence
				 company-sort-by-backend-importance))
    (message "Company - Loaded")

    ;; Start mode globally
    (add-hook 'after-init-hook 'global-company-mode)))

(provide 'init-company)
