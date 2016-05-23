;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Org Mode Package
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package org
  :ensure t
  :init
  (progn
    ;; FIXME
    (setq org-return-follows-link t)

    ;; Support to languages in #-begin_src #end_src code
    (org-babel-do-load-languages
     'org-babel-load-languages
     '(
       (sh . t)
       (python . t)
       (R . t)
       (ruby . t)
       (ditaa . t)
       (dot . t)
       (octave . t)
       (sqlite . t)
       (perl . t)
       (latex . t)))
    
    ;; Key binding
    (global-set-key (kbd "C-c l") 'org-store-link)
    (global-set-key (kbd "C-c a") 'org-agenda)
    (global-set-key (kbd "C-c r") 'org-capture)))

(setq org-structure-template-alist
      '(("l"
	 "#+begin_src emacs-lisp\n?\n#+end_src"
	 "<src lang=\"emacs-lisp\">             \n?\n</src>")
	("t"
	 "#+begin_src text\n?\n#+end_src"
	 "<src lang=\"text\">\n?\n</src>")))

;; Set org directory
(setq org-directory "~/git/org")

;; Set where captured notes will be stored
(setq org-default-notes-file "~/git/org/organizer.org")

(provide 'init-orgmode)




