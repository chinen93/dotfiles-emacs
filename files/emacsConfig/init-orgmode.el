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

  ;; Get the Agenda indexed by 'b'
  (org-agenda nil "b")

  ;; Remove other windows so this is the only one visible
  (delete-other-windows))
;; defun my-week-and-todo-list END


;; defun pc-study/update-timestamp BEGIN
(defun my-update-org-timestamp ()
  "Search for the string 'DATE-UPDATED' and chage the inactive 
timestamp after it."

  ;; Save excursion so the pointer isn't changed
  (save-excursion

    ;; Go to the first positon in the buffer
    (goto-char (point-min))

    ;; Search for the string DATE-UPDATED: [2017-06-13 Tue])
    (if (not (null (search-forward-regexp "DATE-UPDATED: " nil t)))
	
	;; Save the begin to where to delete.
	(let ((begin (point)))

	  ;; Search for the next ']' the end of a date.
	  (search-forward "]")

	  ;; Delete the date described as [year-month=day DayofWeek]
	  (delete-region begin (point))

	  ;; Insert date of today
	  (org-insert-time-stamp (current-time) nil t))

      ;; Text is not found: Message and do nothing
      (message "DATE-UPDATED does not exist in this buffer"))))
;; defun pc-study/update-timestamp END

(defun my-orgp ()
  "Check this file is an org file, is it is execute some functions"

  ;; Add hook before save
  (add-hook 'before-save-hook 'my-update-org-timestamp))

;; Add hook to org mode
(add-hook 'org-mode-hook 'my-orgp)

(defun my-org-toggle-timestamp(beforeList afterList) 
  "Toggle a time stamp to active and inactive, vice versa"

  ;; Don't change the cursor position
  (save-excursion
    
    ;; Narrow to the begin-end of line
    (narrow-to-region (progn 
			(beginning-of-line)
			(point))
		      (progn
			(end-of-line)
			(point)))

    ;; search for begin-end of DATE
    (let ((begin (search-backward (first beforeList) nil t))
	  (end (search-forward (first (rest beforeList)) nil t)))

      ;; if a DATE is found
      (if (and (not (not begin)) (not (not end)))
	  (progn 

	    ;; change character for the appropriate one
	    (delete-region begin (+ begin 1))
	    (goto-char begin)
	    (insert (first afterList))
	    
	    ;; change character for the appropriate one
	    (goto-char end)
	    (delete-region (- end 1) end)
	    (insert (first (rest afterList))))))

    ;; Widen buffer
    (widen)))


(defun my-org-active-timestamp ()
  "Active a timestamp, change [date] to <date>"
  (interactive)
  
  (my-org-toggle-timestamp '("[" "]") '("<" ">")))


(defun my-org-inactive-timestamp ()
  "Inactive a timestamp, change <date> to [date]"
  (interactive)
  
  (my-org-toggle-timestamp '("<" ">") '("[" "]")))


(provide 'init-orgmode)
