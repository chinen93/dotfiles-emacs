;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Org Mode Package
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'org)

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

;; Templates for source blocks 
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
(setq org-default-notes-file "~/Documents/Capture.org")

;; Set org agenda files
(setq org-agenda-files '("~/Dropbox/Organizador.org"))

;; Record a note when TODO item is DONE
(setq org-log-done 'note)

;; Follows links when press <ENTER> on them
(setq org-return-follows-link t)

;; Hide the leading "*" from the headline
(setq org-startup-indented t
      org-hide-leading-stars t)

;; Set new filter for agenda views
(setq org-agenda-custom-commands
 '(("n" "Agenda and all TODO's"
    ((agenda "" nil)
     (alltodo "" nil))
    nil)
   ("b" "Tasks for the week and Agenda"
    ((tags "day"
	   ;; settings for tags command
	   ;; sort result with TODOS first and DONE last
	   ((org-agenda-sorting-strategy '(todo-state-down))
	    ;; remove tags when displaying
	    (org-agenda-remove-tags t)
	    ;; label this search to "Tarefas da semana"
	    (org-agenda-overriding-header "Tarefas da Dia\n")
	    ;; removes the filename of the task
	    (org-agenda-prefix-format "")
	    ))
     (tags "week"
	   ;; settings for tags command
	   ;; sort result with TODOS first and DONE last
	   ((org-agenda-sorting-strategy '(todo-state-down))
	    ;; remove tags when displaying
	    (org-agenda-remove-tags t)
	    ;; label this search to "Tarefas da semana"
	    (org-agenda-overriding-header "Tarefas da Semana\n")
	    ;; removes the filename of the task
	    (org-agenda-prefix-format "")
	    ))
     (agenda "" nil))
    nil)
))

;; Agenda show next 7 days and previous 3 days
(setq org-agenda-span 10
      org-agenda-start-on-weekday nil
      org-agenda-start-day "-3d")

;; defun my-week-and-todo-list BEGIN
(defun my-week-and-todo-list ()
  "Create a list of this week and todo items"
  (interactive)

  (org-agenda nil "b")
  (delete-other-windows)
)
;; defun my-week-and-todo-list END

(provide 'init-orgmode)
