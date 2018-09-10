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

(use-package org-contacts
  :ensure nil
  :after org
  :custom (org-contacts-files '("~/Dropbox/Contacts.org")))

(use-package org-capture
  :ensure nil
  :after org
  :preface
  (defvar my/org-contacts-template "* %(org-contacts-template-name)
:PROPERTIES:
:BIRTHDAY: %^{yyyy-mm-dd}
:EMAIL: %(org-contacts-template-email)
:NOTE: %^{NOTE}
:END:" "Template for org-contacts.")
  :custom
  (org-capture-templates
   `(("c" "Contact" entry (file "~/Dropbox/Contacts.org")
      ,my/org-contacts-template
      :empty-lines 1))))


(provide 'init-orgmodeExtras)
