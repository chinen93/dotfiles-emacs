;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Org Mode Package
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'org)
(require 'org-id)

;; If `org-store-link` is called directly don't create IDs if it already exist
(setq org-id-link-to-org-use-id 'create-if-interactive-and-no-custom-id)

;; Provide refile targets as paths. Level3 headlin = level1/level2/leve3
(setq org-refile-use-outline-path 'file)

;; Place tags directly after headline text, with only one space in between
(setq org-tags-column 0)

;; Load paths to refile in a single go
(setq org-outline-path-complete-in-steps nil)

;; Support to languages in #-begin_src #end_src code
;; (org-babel-do-load-languages
;;  'org-babel-load-languages
;;  '((shell . t)
;;    (python . t)
;;    (latex . t)))

;; Templates for source blocks 
(setq org-structure-template-alist
      '(("l"
	 "#+begin_src emacs-lisp\n?\n#+end_src"
	 "<src lang=\"emacs-lisp\">             \n?\n</src>")
        ("s"
	 "#+begin_src sh\n?\n#+end_src"
	 "<src lang=\"shell\">             \n?\n</src>")
	("t"
	 "#+begin_src text\n?\n#+end_src"
	 "<src lang=\"text\">\n?\n</src>")))

;; Set org directory
(setq org-directory "~/git/org")

;; Set where captured notes will be stored
(setq org-default-notes-file "~/Documents/Capture.org")

;; Set dropbox folder
(setq org-dropbox-folder "~/Dropbox")

;; Set org agenda files
(setq org-agenda-files (list (concat org-dropbox-folder "/Organizador.org")
			     (concat org-dropbox-folder "/Notes")))

;; Record a note when TODO item is DONE
(setq org-log-done 'note)
(setq org-log-repeat 'note)
(add-to-list 'org-modules "org-habit")
(setq org-todo-keywords
      '((sequence "TODO(t)" "WORKING(w@/!)" "|" "DONE(d!)" "CANCELED(c@)")))

;; Follows links when press <ENTER> on them
(setq org-return-follows-link t)

;; Hide the leading "*" from the headline
(setq org-startup-indented t
      org-hide-leading-stars t)

;; Set new filter for agenda views
(setq org-agenda-custom-commands
 '(
   ;; Custom command to show done task from previous week
   ("b" "DONE from this week" 
    ;; Use normal agenda
    ((agenda ""
	     ;; Put some configurations on this agenda
	     ;; Show closed tasks and show 7 days starting today
	     ((org-agenda-log-mode-items '(closed))
	      (org-agenda-span 7)
	      (org-agenda-start-day "-6d")
	      (org-agenda-show-log t)
	      (org-agenda-window-setup 'other-window)))) 
    nil)
   ;; Custom agenda that show all the TODO tasks
   ("n" "Agenda and all TODO's"
    ((agenda "" nil)
     (alltodo "" nil))
    nil)

   ("w" "Working on tasks"
    ((todo "WORKING" nil))
    nil nil)
))

;; Agenda show next 7 days and previous 3 days
(setq org-agenda-span 10
      org-agenda-start-on-weekday nil
      org-agenda-start-day "-3d")


(setq org-icalendar-timezone "America/Sao_Paulo")


;; ===================================================================
;; Functions
;; ===================================================================

;; defun org-id-complete-link BEGIN
(defun org-id-complete-link (&optional arg)
  "Create an id: link using completion"
  (concat "id:"
          (org-id-get-with-outline-path-completion)))
;; defun org-id-complete-link END


;; defun my-week-and-todo-list BEGIN
(defun my-week-and-todo-list ()
  "Create a list of this week and todo items"
  (interactive)

  ;; Get the Agenda indexed by 'n'
  (org-agenda nil "n")

  ;; Remove other windows so this is the only one visible
  (delete-other-windows)
)
;; defun my-week-and-todo-list END


;; defun my-update-org-timestamp BEGIN
(defun my-update-org-timestamp ()
  "Search for the string 'DATE-UPDATED' and chage the inactive 
timestamp after it."

  ;; Save excursion so the pointer isn't changed
  (save-excursion

    ;; Go to the first positon in the buffer
    (goto-char (point-min))

    ;; Search for the string DATE-UPDATED: [2018-02-10 Sat])
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
;; defun my-update-org-timestamp END


;; defun my-add-ids-to-headings BEGIN
(defun my-add-ids-to-all-headings ()
  "Insert ids to every heading in the file. If it already has one do nothing"
  (interactive)
  (save-excursion
    (goto-char (point-max))
    (while (outline-previous-heading)
      (org-id-get-create))))
;; defun my-add-ids-to-headings END


;; defun my-org-toggle-timestamp BEGIN
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
;; defun my-org-toggle-timestamp END


;; defun my-org-active-timestamp BEGIN
(defun my-org-active-timestamp ()
  "Active a timestamp, change [date] to <date>"
  (interactive)
  
  (my-org-toggle-timestamp '("[" "]") '("<" ">")))
;; defun my-org-active-timestamp END


;; defun my-org-inactive-timestamp BEGIN
(defun my-org-inactive-timestamp ()
  "Inactive a timestamp, change <date> to [date]"
  (interactive)
  
  (my-org-toggle-timestamp '("<" ">") '("[" "]")))
;; defun my-org-inactive-timestamp END

;; ===================================================================
;; Hooks
;; ===================================================================
;; List of hooks for org-mode: http://orgmode.org/tmp/worg/org-configs/org-hooks.html

;; defun my-org-hook-function BEGIN
(defun my-org-hook-function ()
  "Check this file is an org file, is it is execute some functions"

  ;; Add hook before save
  (add-hook 'before-save-hook 
	    (when (and (eq major-mode 'org-mode)
		       (eq buffer-read-only nil))
	      'my-update-org-timestamp)))
;; defun my-org-hook-function END

;; Add hook to org mode
(add-hook 'org-mode-hook 'my-org-hook-function)
(add-hook 'org-insert-heading-hook 'org-id-get-create)


(provide 'init-orgmode)
