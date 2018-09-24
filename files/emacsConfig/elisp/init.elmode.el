;; Don't edit this file, edit ~/emacsConfig/init-orgmode.org instead ...

  (require 'org)

  ;; Set org directory
  (setq org-directory "~/git/org")

  ;; Set dropbox folder
  (setq org-dropbox-folder "~/Dropbox")

  ;; Only set this org variable if there no other variable for the dropbox folder
  (unless (null my-dropbox-folder)
    (setq org-dropbox-folder my-dropbox-folder))
  (require 'org-id)

  ;; If `org-store-link` is called directly don't create IDs if it already exist
  (setq org-id-link-to-org-use-id 'create-if-interactive-and-no-custom-id)

  ;; Place tags directly after headline text, with only one space in between
  (setq org-tags-column 0)

  ;; Record a note when TODO item is DONE
  (setq org-log-done 'note)
  (setq org-log-repeat 'note)
  (setq org-icalendar-timezone "America/New_York")

  (setq org-todo-keywords
        '((sequence "TODO(t)" "WORKING(w@/!)" "|" "DONE(d!)" "CANCELED(c@)")))

  ;; Follows links when press <ENTER> on them
  (setq org-return-follows-link t)

  ;; Hide the leading "*" from the headline
  (setq org-startup-indented t
        org-hide-leading-stars t)
  ;; Provide refile targets as paths. Level3 headlin = level1/level2/leve3
  (setq org-refile-use-outline-path 'file)

  ;; Load paths to refile in a single go
  (setq org-outline-path-complete-in-steps nil)

  (setq org-refile-targets
        '((nil :maxlevel . 3)))
  ;; Set org agenda files
  (setq org-agenda-files 
        (list (concat org-dropbox-folder "/Organizador.org")
              (concat org-dropbox-folder "/Notes/Projetos.org")
              (concat org-dropbox-folder "/Contacts.org")))

  (setq org-agenda-include-all-todo nil)
  (setq org-agenda-skip-scheduled-if-done t)
  (setq org-agenda-skip-deadline-if-done t)
  (setq org-agenda-columns-add-appointments-to-effort-sum t)
  (setq org-agenda-custom-commands nil)
  (setq org-agenda-default-appointment-duration 60)
  (setq org-agenda-mouse-1-follows-link t)
  (setq org-agenda-skip-unavailable-files t)
  (setq org-agenda-use-time-grid nil)

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
           ((agenda ""
                    ((org-agenda-span 16)
                     (org-agenda-start-day "-2d")))
            (alltodo "" nil))
           nil)

          ;; Custom agenda to show working todo
          ("w" "Working on tasks"
           ((agenda ""
                    ((org-agenda-span 4)
                     (org-agenda-start-day "-1d")))
            (todo "WORKING" nil))
           nil nil)

          ;; Custom agenda to show agenda and todo for every note
          ("l" "Agenda and all TODO's"
           ((agenda ""
                    ((org-agenda-files '("~/Dropbox/Organizador.org" "~/Dropbox/Notes"))))
            (alltodo ""
                     ((org-agenda-files '("~/Dropbox/Organizador.org" "~/Dropbox/Notes")))))
           nil)
          ))

  ;; Agenda show next 7 days and previous 3 days
  (setq org-agenda-span 10
        org-agenda-start-on-weekday nil
        org-agenda-start-day "-3d")

  ;; Templates for source blocks
  (setq org-structure-template-alist
        '(("l"
           "#+BEGIN_SRC emacs-lisp\n?\n#+END_SRC"
           "<src lang=\"emacs-lisp\">             \n?\n</src>")
          ("s"
           "#+BEGIN_SRC sh\n?\n#+END_SRC"
           "<src lang=\"shell\">             \n?\n</src>")
          ("t"
           "#+BEGIN_SRC text\n?\n#+END_SRC"
           "<src lang=\"text\">\n?\n</src>")))
  ;; Support to languages in #-begin_src #end_src code
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (shell . t)
     (python . t)
     (latex . t)))
  (defun my-week-and-todo-list ()
      "Create a list of this week and todo items"
      (interactive)

      ;; Add a theme.
      ;; (load-theme 'tango)

      ;; Get the Agenda indexed by 'n'
      (org-agenda nil "n")

      ;; Remove other windows so this is the only one visible
      (delete-other-windows))
  (defun my-update-org-timestamp ()
    "Search for the string 'DATE-UPDATED' and chage the inactive
  timestamp after it."

    ;; Check to see if this is an Org mode file
    (when (and (eq major-mode 'org-mode)
               (eq buffer-read-only nil))

      ;; Save excursion so the pointer isn't changed
      (save-excursion

        ;; Go to the first positon in the buffer
        (goto-char (point-min))

        ;; Search for the string DATE-UPDATED: [2018-09-23 Sun])
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
          (message "DATE-UPDATED does not exist in this buffer")))))

  (defun my-add-ids-to-all-headings ()
    "Insert ids to every heading in the file. If it already has one do nothing"
    (interactive)
    (save-excursion
      (goto-char (point-max))
      (while (outline-previous-heading)
        (org-id-get-create))))

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
  ;; https://emacs.stackexchange.com/questions/30303/how-to-remove-org-id-drawer-location-file-entry
  (defun org-id-remove-entry ()
  "Remove/delete the ID entry and update the databases.
  Update the `org-id-locations' global hash-table, and update the
  `org-id-locations-file'.  `org-id-track-globally' must be `t`."
  (interactive)
    (save-excursion
      (org-back-to-heading t)
      (when (org-entry-delete (point) "ID")
        (org-id-update-id-locations nil 'silent))))
  (defun my-org-hook-function ()
    "Check this file is an org file, is it is execute some functions"

    ;; Add hook before save
    (add-hook 'before-save-hook 'my-update-org-timestamp))


  ;; Add hook to org mode
  (add-hook 'org-mode-hook 'my-org-hook-function)
  (add-hook 'org-insert-heading-hook 'org-id-get-create)

  (add-hook 'org-mode-hook
            ;; Create hook when org mode is enabled
            (lambda()
              (visual-line-mode t)
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
        :empty-lines 1)

       ;("p" "Project" entry (file "~/Dropbox/Notes/Projetos.org"))
       )))
