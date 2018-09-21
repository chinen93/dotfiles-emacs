;; Don't edit this file, edit ~/emacsConfig/misc.org instead ...

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Caculator Package
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


  (provide 'init-calcmode)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Company package
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (require-package 'company)

  ;; Set delay to start completition
  (setq company-idle-delay 0.2)
  (setq company-echo-delay 0)

  ;; Keep the return of company as-is
  (setq company-dabbrev-downcase nil)

  ;; Minimimum size to start to search for match
  (setq company-minimum-prefix-length 3)
  (setq company-selection-wrap-around t)

  ;; Sort matches by occurrence and backend importance
  (setq company-transformers '(company-sort-by-occurrence
                               company-sort-by-backend-importance))

  (message "Company - Loaded")

  ;; Start mode globally
  (add-hook 'after-init-hook 'global-company-mode)

  (provide 'init-company)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Diminish
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (use-package diminish
    :defer 3
    :ensure t
    :config
    (progn
      (diminish 'ivy-mode)
      (diminish 'company-mode)
      (diminish 'elmacro-mode)
      (diminish 'yas-global-mode)
      (diminish 'yas-minor-mode)
      (diminish 'flycheck-mode " FC")
      (diminish 'auto-revert-mode)

      (diminish 'helm)))

  (provide 'init-diminish)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Dired Package
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  ;; -a : show all entries even those "hidden".
  ;; -l : use a long listing format.
  ;; -H : follow symbolic links.
  ;; --group-directories-first : directory before files.
  (setq dired-listing-switches "-alH --group-directories-first")

  (defun xah-dired-mode-setup ()
    "to be run as hook for `dired-mode'."
    (dired-hide-details-mode 1))
  (add-hook 'dired-mode-hook 'xah-dired-mode-setup)

  (provide 'init-dired)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Expand Region Package
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (use-package expand-region
    :defer 2
    :ensure t
    :config
    (progn
      (message "Expand Region - Loaded")
      ;; Bind key to command
      (global-set-key (kbd "C-=") 'er/expand-region)))

  (provide 'init-expandRegion)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; File Modes
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (setq auto-mode-alist
        (append
         ;; File name (within directory) starts with a dot.
         '((".bashrc" . shell-script-mode)
           (".bash_aliases" . shell-script-mode)
           (".bash_profile" . shell-script-mode)
           (".screenrc" . shell-script-mode)
           (".ledgerrc" . shell-script-mode)

           ;; css mode
           (".scss" . css-mode)

           ;; File name has no dot.
           ("/[^\\./]*\\'" . fundamental-mode)

           ;; File name ends in ‘.C’.
           ("\\.C\\'" . c++-mode))
         auto-mode-alist))

  (provide 'init-filesMode)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; First Initialization
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (require 'server)
  (unless (server-running-p)
    (server-start))

  (provide 'init-firstInit)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Flycheck
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (use-package flycheck
    :defer 2
    :ensure t
    :config
    (progn
      (message "Flycheck - Loaded")

      ;; Flycheck gets to be a bit much when warning about checkdoc issues.
      (setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc))

      (add-hook 'prog-mode-hook 'flycheck-mode)
      ))

  (provide 'init-flycheck)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Fun Packages
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  ;; (use-package wotd
  ;;   :ensure t
  ;;   :config
  ;;   (progn
  ;;     (message "Word of the day - Loaded")
  ;;     ))

  ;; (use-package typit
  ;;   :ensure t
  ;;   :config
  ;;   (progn
  ;;     (message "Typit - Loaded")
  ;;     ))

  ;; (use-package focus
  ;;   :ensure t
  ;;   :config
  ;;   (progn
  ;;     (message "Focus - Loaded")
  ;;     ))

  ;; (use-package google-translate
  ;;   :ensure t
  ;;   :config
  ;;   (progn
  ;;     (message "Google Translate - Loaded")

  ;;     (require 'google-translate-smooth-ui)

  ;;     (setq google-translate-translation-directions-alist
  ;;           '( ("en" . "pt") ("pt" . "en")))
  ;;     ))

  ;; (use-package google-this
  ;;   :ensure t
  ;;   :config
  ;;   (progn
  ;;     (message "Google This - Loaded")
  ;;     ))

  (use-package goto-addr
    ;; http://xenodium.com/#actionable-urls-in-emacs-buffers
    :hook ((compilation-mode . goto-address-mode)
           (prog-mode . goto-address-prog-mode)
           (eshell-mode . goto-address-mode)
           (shell-mode . goto-address-mode))
    :bind (:map goto-address-highlight-keymap
                ("<RET>" . goto-address-at-point)
                ("M-<RET>" . newline))
    :commands (goto-address-prog-mode
               goto-address-mode))

  (provide 'init-fun)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; GGTAGS
  ;;
  ;; DATE_CREATE: 2018-09-06
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (use-package ggtags
    :defer t
    :ensure t
    :config
    (progn
      (message "GGTAGS - Loaded")
      (ggtags-mode 1)
      (add-hook 'c-mode-common-hook
                (lambda ()
                  (when (derived-mode-p 'c-mode 'c++-mode 'java-mode 'asm-mode)
                    (ggtags-mode 1))))))

  (provide 'init-ggtags)
  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; ;; Helm Package
  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  ;; (use-package helm
  ;;   :defer 2
  ;;   :ensure t
  ;;   :diminish helm-mode
  ;;   :init
  ;;   (progn
  ;;     (require 'helm-config)
  ;;     (message "Helm - Loaded")

  ;;     ;; set max number of candidates
  ;;     (setq helm-candidate-number-limit 100)

  ;;     ;; From https://gist.github.com/antifuchs/9238468
  ;;     ;; update fast sources immediately (doesn't).
  ;;     (setq helm-idle-delay 0.0)

  ;;     ;; this actually updates things
  ;;     (setq helm-input-idle-delay 0.01)

  ;;     ;; reeeelatively quickly.
  ;;     (setq helm-yas-display-key-on-candidate t)
  ;;     (setq helm-quick-update t)

  ;;     ;; FIXME
  ;;     (setq helm-M-x-requires-pattern nil)
  ;;     (setq helm-split-window-in-side-p t)
  ;;     (setq helm-ff-skip-boring-files t)

  ;;     ;; start mode
  ;;     (helm-mode)

  ;;     ;; key binding
  ;;     (global-set-key (kbd "C-c h") 'helm-mini)
  ;;     (global-set-key (kbd "C-h a") 'helm-apropos)
  ;;     (global-set-key (kbd "C-x b") 'helm-buffers-list)
  ;;     (global-set-key (kbd "C-x C-b") 'helm-buffers-list)
  ;;     (global-set-key (kbd "C-x C-f") 'helm-find-files)
  ;;     (global-set-key (kbd "M-y") 'helm-show-kill-ring)
  ;;     (global-set-key (kbd "M-x") 'helm-M-x)

  ;;     ;; uses my key prefix
  ;;     (global-set-key (kbd "M-v M-f") 'helm-find-files)
  ;;     (global-set-key (kbd "M-v M-p") 'helm-bookmarks)
  ;;     ))

  ;; (provide 'init-helm)
  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; ;; Helm Describe Key Package
  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  ;; (use-package helm-descbinds
  ;;   :defer 2
  ;;   :ensure t
  ;;   :init
  ;;   (progn
  ;;     (message "Helm Describe Bindings - Loaded")))

  ;; (provide 'init-helmDescribeKey)
  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; ;; Helm Swoop Package
  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  ;; (use-package helm-swoop
  ;;   :defer 2
  ;;   :ensure t
  ;;   :init
  ;;   (progn
  ;;     (message "Helm Swoop - Loaded")

  ;;     ;; Make Swoop faster
  ;;     (setq helm-swoop-speed-or-color t)

  ;;     ;; make swoop in actual window
  ;;     (setq helm-swoop-split-with-multiple-windows t)

  ;;     ;; Bind key
  ;;     (global-set-key (kbd "C-f") 'helm-swoop)))


  ;; (provide 'init-helmSwoop)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; History
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  ;; Set directory to hold history
  (setq savehist-file "~/.emacs.d/savehist")

  ;; Start mode
  (savehist-mode 1)

  ;; FIXME
  (setq history-length t)

  ;; Delete duplicated history
  (setq history-delete-duplicates t)

  ;; Save minibuffer history
  (setq savehist-save-minibuffer-history 1)

  ;; Save hist for kill rings, search rings and regex search rings
  (setq savehist-additional-variables
        '(kill-ring
          search-ring
          regexp-search-ring))

  (provide 'init-history)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Hooks
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  ;; org-mode
  (add-hook 'org-mode-hook
            ;; Create hook when org mode is enabled
            (lambda()
              (visual-line-mode t)
              ))



  ;; python-mode
  (add-hook 'python-hook
            (progn 
              ;; use the python 3.1
              (setq py-python-command "/usr/bin/python3.1")

              (use-package company-jedi
                :ensure t
                :config (progn 
                          (add-to-list 'company-backends 'company-jedi)))

              ;; progn
              )

            ;; add-hook 'python-mode
            )


  (provide 'init-hooks)

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Javascript Configuration
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (use-package js2-mode
    :defer 2
    :ensure t
    :config
    (progn
      ;; configs here
      (message "Javascript Configuration - Loaded")

      ;; number of spaces when identing
      (setq indent-tabs-mode nil)
      (setq js2-basic-offset 2)

      ;; load this mode when loading .js files
      (add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
      (add-to-list 'auto-mode-alist '("\\.json\\'" . js2-mode))))

  (provide 'init-javascript)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Key Frequence
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (use-package keyfreq
    :defer 2
    :ensure t
    :config
    (progn
      (message "Keyfreq - Loaded")

      ;; Commands that are not listed in (keyfreq-show)
      (setq keyfreq-excluded-commands
            '(self-insert-command
              abort-recursive-edit
              backward-char
              backward-delete-char-untabify
              c-electric-backspace
              company-ignore
              delete-backward-char
              forward-char
              helm-next-line
              helm-previous-line
              left-char
              mouse-drag-region
              mouse-set-point
              mwheel-scroll
              next-line
              org-delete-backward-char
              org-self-insert-command
              previous-line
              right-char))

      ;; Start keyfreq mode
      (keyfreq-mode 1)

      ;; Star key freq auto sabex
      (keyfreq-autosave-mode 1)))


  (provide 'init-keyfreq)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Macro
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (use-package elmacro
    :defer 2
    :ensure t
    :config
    (progn
      (message "Elmacro - Loaded")
      (elmacro-mode)))

  (provide 'init-macro)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Markdown Mode
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (use-package markdown-mode
    :defer 2
    :ensure t
    :commands (markdown-mode gfm-mode)
    :mode (("README\\.md\\'" . gfm-mode)
           ("\\.md\\'" . markdown-mode)
           ("\\.markdown\\'" . markdown-mode))
    :init (setq markdown-command "multimarkdown"))


  (provide 'init-markdown)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Multiple Cursor Package
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (use-package multiple-cursors
    :defer 1
    :ensure t
    :config
    (progn
      (global-set-key (kbd "C->") 'mc/mark-next-like-this)
      (global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
      ))

  (provide 'init-multipleCursor)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Nov Package
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (use-package nov
    :defer 2
    :ensure t
    :config
    (progn
      (message "Nov Mode - Loaded")

      (setq nov-text-width most-positive-fixnum)
      (add-hook 'nov-mode-hook 'visual-line-mode)

      (setq nov-text-width 80)

      (defun my-nov-font-setup ()
        (face-remap-add-relative
         'variable-pitch
         :family "Liberation Serif"
         :height 1.0))

      (add-hook 'nov-mode-hook 'my-nov-font-setup)

      (add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode))

      ))

  (provide 'init-nov)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Nyan Cat Package
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (use-package nyan-mode
    :ensure t
    :config
    (progn
      (message "Nyan Mode - Loaded")

      ;; Max length of the nyan rainbow trail
      (setq nyan-bar-length 10)

      ;; start nyan mode
      (nyan-mode 1)))

  (provide 'init-nyanCat)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Org Mode Package
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (require 'org)
  (require 'org-id)
  (require 'org-contacts)

  ;; Set org directory
  (setq org-directory "~/git/org")

  ;; Set dropbox folder
  (setq org-dropbox-folder "~/Dropbox")

  ;; Only set this org variable if there no other variable for the dropbox folder
  (unless (null my-dropbox-folder)
    (setq org-dropbox-folder my-dropbox-folder))

  ;; Set org agenda files
  (setq org-agenda-files 
        (list (concat org-dropbox-folder "/Organizador.org")
              (concat org-dropbox-folder "/Notes/Projetos.org")
              (concat org-dropbox-folder "/Contacts.org")))

  ;; If `org-store-link` is called directly don't create IDs if it already exist
  (setq org-id-link-to-org-use-id 'create-if-interactive-and-no-custom-id)

  ;; Place tags directly after headline text, with only one space in between
  (setq org-tags-column 0)

  ;; Provide refile targets as paths. Level3 headlin = level1/level2/leve3
  (setq org-refile-use-outline-path 'file)

  ;; Load paths to refile in a single go
  (setq org-outline-path-complete-in-steps nil)

  (setq org-refile-targets
        '((nil :maxlevel . 3)))

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

  (setq org-agenda-include-all-todo nil)
  (setq org-agenda-skip-scheduled-if-done t)
  (setq org-agenda-skip-deadline-if-done t)
  (setq org-agenda-columns-add-appointments-to-effort-sum t)
  (setq org-agenda-custom-commands nil)
  (setq org-agenda-default-appointment-duration 60)
  (setq org-agenda-mouse-1-follows-link t)
  (setq org-agenda-skip-unavailable-files t)
  (setq org-agenda-use-time-grid nil)

  ;; Record a note when TODO item is DONE
  (setq org-log-done 'note)
  (setq org-log-repeat 'note)
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


  (setq org-icalendar-timezone "America/New_York")


  ;; ===================================================================
  ;; Functions
  ;; ===================================================================

  ;; defun my-week-and-todo-list BEGIN
  (defun my-week-and-todo-list ()
    "Create a list of this week and todo items"
    (interactive)

    ;; Add a theme.
    ;; (load-theme 'tango)

    ;; Get the Agenda indexed by 'n'
    (org-agenda nil "n")

    ;; Remove other windows so this is the only one visible
    (delete-other-windows))
  ;; defun my-week-and-todo-list END


  ;; defun my-update-org-timestamp BEGIN
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

        ;; Search for the string DATE-UPDATED: [2018-09-21 Fri])
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

  ;; ===================================================================
  ;; Hooks
  ;; ===================================================================
  ;; List of hooks for org-mode: http://orgmode.org/tmp/worg/org-configs/org-hooks.html

  ;; defun my-org-hook-function BEGIN
  (defun my-org-hook-function ()
    "Check this file is an org file, is it is execute some functions"

    ;; Add hook before save
    (add-hook 'before-save-hook 'my-update-org-timestamp))
  ;; defun my-org-hook-function END

  ;; Add hook to org mode
  (add-hook 'org-mode-hook 'my-org-hook-function)
  (add-hook 'org-insert-heading-hook 'org-id-get-create)


  (provide 'init-orgmode)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Org Mode Languages Package
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  ;; Support to languages in #-begin_src #end_src code
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (shell . t)
     (python . t)
     (latex . t)))

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


  (provide 'init-orgmodeExtras)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Programming Configuration
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (defun infer-indentation-style ()
    ;; if our source file uses tabs, we use tabs, if spaces spaces, and if
    ;; neither, we use the current indent-tabs-mode
    (let ((space-count (how-many "^  " (point-min) (point-max)))
          (tab-count (how-many "^\t" (point-min) (point-max))))
      (if (> space-count tab-count) (setq indent-tabs-mode nil))
      (if (> tab-count space-count) (setq indent-tabs-mode t))))

  (add-hook 'prog-mode-hook 'infer-indentation-style)


  ;; (use-package haskell-mode
  ;;   :ensure t
  ;;   :config
  ;;   (progn
  ;;     (message "Haskell mode - Loaded")
  ;;     ))


  (provide 'init-programming)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Rainbow Package
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (use-package rainbow-delimiters
    :defer 2
    :ensure t
    :config
    (progn
      (message "Rainbow Delimiter - Loaded")

      (require 'rainbow-delimiters)
      (add-hook 'prog-mode-hook 'rainbow-delimiters-mode)
      ))

  (provide 'init-rainbow)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Regular Expression
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (use-package re-builder
    :defer t
    :ensure t
    :config
    (progn
      (message "Rebuilder - Loaded")
      ;; FIXME
      (setq reb-re-synstax 'string)))

  (provide 'init-regex)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Secrets
  ;;
  ;; DATE_CREATE: 2018-08-24
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (require 'epa-file)
  (epa-file-enable)

  ;; symmetric encryption only
  (setq epa-file-select-keys nil)

  (setq epg-gpg-program "gpg2")
  (setenv "GPG_AGENT_INFO" nil)

  (provide 'init-secrets)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Themes
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  ;; Load theme
  (use-package zenburn-theme
    :defer 1
    :ensure t)

  (use-package monokai-theme
    :defer 1
    :ensure t
    :config
    (progn
      (message "Monokai Theme - Loaded")))

  (defun sakshamsharma-setTheme (themeName)
    "Set the theme to THEMENAME."
    (interactive "sWhat theme do you want to use? ")
    (when (display-graphic-p)
      (load-theme (intern themeName) t)))

  (defun sakshamsharma-setFont (fntName)
    "Set the font to FNTNAME."
    (interactive "sWhat font name do you want to set? ")
    (set-face-attribute 'default nil
                        :family fntName
                        :height 105
                        :weight 'normal
                        :width 'normal))

  (defun sakshamsharma-frameActions ()
    "Do actions to set up appearance of frame."
    (interactive)
    (let ((myTheme "monokai") (myFont "DejaVu Sans Mono"))
      ;; (disableBells)
      (sakshamsharma-setTheme myTheme)
      (sakshamsharma-setFont myFont)))

  (sakshamsharma-frameActions)

  (provide 'init-themes)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Try
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (use-package try
    :defer 2
    :ensure t
    :config
    (progn
      (message "Try - Loaded")))

  (provide 'init-try)
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
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; YASnippet Package
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (require-package 'yasnippet)

  (message "Yasnippet - Loaded")

  ;; Surppress yasnippet backquote changes
  (setq warning-suppress-types '(yasnippet backquote-change))

  ;; Controls indenting applied to snippets.
  (setq yas-indent-line 'fixed)

  ;; Change add Directories when looking for snippets
  (setq yas-snippet-dirs
        ;; Personal Collection
        '("~/emacsSnippets"))

  ;; Undefine default keys binding
  ;; (define-key yas-minor-mode-map (kbd "<tab>") nil)
  ;; (define-key yas-minor-mode-map (kbd "TAB") nil)

  ;; Create new key binding
  ;; (define-key yas-minor-mode-map (kbd "<C-tab>") 'yas-expand)
  ;; (define-key yas-minor-mode-map (kbd "C-v s") 'yas-insert-snippet)

  ;; Enable yasnippet mode globally
  (yas-global-mode)

  ;; (defun shk-yas/helm-prompt (prompt choices &optional display-fn)
  ;;   "Use helm to select a snippet. Put this into `yas-prompt-functions.'"
  ;;   (interactive)
  ;;   (setq display-fn (or display-fn 'identity))
  ;;   (if (require 'helm-config)
  ;;       (let (tmpsource cands result rmap)
  ;;      (setq cands (mapcar (lambda (x) (funcall display-fn x)) choices))
  ;;      (setq rmap (mapcar (lambda (x) (cons (funcall display-fn x) x)) choices))
  ;;      (setq tmpsource
  ;;            (list
  ;;             (cons 'name prompt)
  ;;             (cons 'candidates cands)
  ;;             '(action . (("Expand" . (lambda (selection) selection))))
  ;;             ))
  ;;      (setq result (helm-other-buffer '(tmpsource) "*helm-select-yasnippet"))
  ;;      (if (null result)
  ;;          (signal 'quit "user quit!")
  ;;        (cdr (assoc result rmap))))
  ;;     nil))

  ;; (setq yas-prompt-functions '(shk-yas/helm-prompt))

  (provide 'init-yasnippet)
