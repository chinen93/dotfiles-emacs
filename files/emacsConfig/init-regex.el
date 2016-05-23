;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Regular Expression
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package re-builder
  :ensure t
  :config
  (progn
    (message "Rebuilder - Loaded")
    ;; FIXME
    (setq reb-re-synstax 'string)))

(provide 'init-regex)
