;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Company package
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package company
  :ensure t
  :config
  (progn

    ;; Set delay to start completition
    (setq company-idle-delay 0.2)
    (setq company-echo-delay 0)

    ;; Keep the return of company as-is
    (setq company-dabbrev-downcase nil)

    ;; Minimimum size to start to search for match
    (setq company-minimum-prefix-length 2)
    (setq company-selection-wrap-around t)

    ;; Sort matches by occurrence and backend importance
    (setq company-transformers '(company-sort-by-occurrence
				 company-sort-by-backend-importance))

    (message "Company - Loaded")

    ;; Start mode globally
    (add-hook 'after-init-hook 'global-company-mode)))

(provide 'init-company)
