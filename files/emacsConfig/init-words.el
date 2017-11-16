;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Words Package
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(use-package define-word
  :ensure t
  :config
  (progn
    (message "Define Word - Loaded")
    ))


(use-package string-inflection
  :ensure t
  :config
  (progn
    (message "String Inflection [camelCase => snake_case] - Loaded")
    ))


(provide 'init-words)
