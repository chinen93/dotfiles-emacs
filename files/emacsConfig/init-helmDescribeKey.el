;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Helm Describe Key Package
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package helm-descbinds
  :defer 2
  :ensure t
  :init
  (progn
    (message "Helm Describe Bindings - Loaded")))

(provide 'init-helmDescribeKey)
