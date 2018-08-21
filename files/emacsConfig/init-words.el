;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Words Package
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(use-package define-word
  :defer 2
  :ensure t
  :config
  (progn
    (message "Define Word - Loaded")
    ))


(use-package string-inflection
  :defer 2
  :ensure t
  :config
  (progn
    (message "String Inflection [camelCase => snake_case] - Loaded")
    ))


(provide 'init-words)
