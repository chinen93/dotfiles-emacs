;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; COMPANY configuration
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require-package 'company)

(setq company-idle-delay 0)
(setq company-echo-delay 0)
(setq company-dabbrev-downcase nil)

;; sort matches by occurrence and backend importance
(setq company-transformers '(company-sort-by-occurrence
			     company-sort-by-backend-importance))

(add-hook 'after-init-hook 'global-company-mode)

(provide 'init-windows-company)
