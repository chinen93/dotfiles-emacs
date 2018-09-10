;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; GGTAGS
;;
;; DATE_CREATE: 2018-09-06
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package ggtags
  :defer t
  :ensure t
  :config
  (progn
    (message "GGTAGS - Loaded")
    (ggtags-mode 1)
    (add-hook 'c-mode-common-hook
	      (lambda ()
		(when (derived-mode-p 'c-mode 'c++-mode 'java-mode 'asm-mode)
		  (ggtags-mode 1))))))

(provide 'init-ggtags)
