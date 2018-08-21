;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Org Mode Languages Package
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package ob-python
  :defer 2
  :ensure org-plus-contrib
  :commands (org-babel-execute:python))

(use-package ob-shell
  :defer 2
  :ensure org-plus-contrib
  :commands
  (org-babel-execute:sh
   org-babel-expand-body:sh

   org-babel-execute:bash
   org-babel-expand-body:bash)

  :config
  (progn
    (add-to-list 'org-babel-load-languages '(shell . t))
    ))

(provide 'init-orgmodeLanguages)
