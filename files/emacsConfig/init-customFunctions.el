;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Custom Functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;=========================================================
;; defun my-load-hydra-helm-windows
;;=========================================================
(defun my-load-hydra-helm-windows ()
  "Load hydra and helm configurations properly on Windows"
  (interactive)

  (load-file "~/emacsConfig/init-hydra.el")
  (load-file "~/emacsConfig/init-helm.el"))


;;=========================================================
;; defun my-trim-right
;;=========================================================
(defun my-trim-right ()
  "Remove all the whitespaces right to the last char"
  (interactive)

  ;; go to end of line
  (end-of-line)

  ;; move point backward, stopping after a char not in STRING
  (skip-chars-backward "\t ")

  ;; kill rest of line including the newline
  (kill-line)

  ;; create a new line
  (newline)

  ;; go to previous line and go to the end of it
  (previous-line)
  (end-of-line))


;; ===================================================================
;; Functions from Internet
;; ===================================================================

;;=========================================================
;; defun xah-cut-line-or-region
;;=========================================================
(defun xah-cut-line-or-region ()
  "Cut current line, or text selection.
   When `universal-argument' is called first,
   cut whole buffer (respects `narrow-to-region').
   URL `http://ergoemacs.org/emacs/emacs_copy_cut_current_line.html'
   Version 2015-06-10"

  (interactive)

  ;; check if exist a prefix argument for this command
  (if current-prefix-arg

      ;; exist a prefix argument: kill whole buffer
      (progn

        ;; this will be the latest kill in the kill ring
        (kill-new
         x

         ;; selected whole buffer
         (buffer-string))

        ;; delete region between:
        (delete-region

         ;; begnning of buffer
         (point-min)

         ;; end of buffer
         (point-max)))

    ;; do not exist a prefix argument: kill some region
    (progn

      ;; check if a region is selected
      (if (use-region-p)

          ;; region selected: kill region between:
          (kill-region

           ;; beginning of the region selected
           (region-beginning)

           ;; end of the region selected
           (region-end)

           ;; kill the region
           t)

        ;; region is not selected: kill region between:
        (kill-region

         ;; beginning of line
         (line-beginning-position)

         ;; end of line
         (line-end-position))))))


;;=========================================================
;; defun xah-new-empty-buffer
;;=========================================================
;; URL: http://ergoemacs.org/emacs/emacs_new_empty_buffer.html
(defun xah-new-empty-buffer ()
  "Open a new empty buffer."
  (interactive)

  ;; (1) bind variables and (2) eval some commands
  (let

      ;; (1) bind variables
      (

       ;; create variable 'my-new-buffer'
       (my-new-buffer

        ;; set variable with the new buffer untitled
        (generate-new-buffer "untitled")))

    ;; (2) eval some commands
    ;; switch to new buffer
    (switch-to-buffer my-new-buffer)

    ;; switch mode of untitle buffer
    (funcall (text-mode))

    ;; set variable buffer-other-save 't'
    (setq buffer-other-save t)))


;;=========================================================
;; defun xah-forward-block
;;=========================================================
;; URL: http://ergoemacs.org/emacs/emacs_move_by_paragraph.html
(defun xah-forward-block (&optional my-n)
  "Move cursor forward to the beginning of the next text block.
   A text block is separated by blank lines"
  (interactive "p")

  ;; (1) bind variables and (2) eval some commands
  (let

      ;; (1) bind some variables
      (


       ;; create variable 'my-n'
       (my-n

        ;; set variable 'my-n'
        (if (null my-n)

            ;; my-n is null: set as 1
            1

          ;; my-n is not null: set as my-n
          my-n)))

    ;; (2) eval some commands
    ;; search for next text block
    (search-forward-regexp

     ;; regular expression to be used
     "\n[ \|\n\|\t]*\n+"

     ;; do not bound the result
     nil

     ;; if fails: move to limit of search and return nil
     "NOERROR"

     ;; repeat my-n times for successive occurrences
     my-n)))


;;=========================================================
;; defun xah-backward-block
;;=========================================================
;; URL: http://ergoemacs.org/emacs/emacs_move_by_paragraph.html
(defun xah-backward-block (&optional my-n)
  "Move cursor backward to previous text block.
See: `xah-forward-block'"
  (interactive "p")

  ;; (1) bind variables and (2) eval some commands
  (let

      ;; (1) bind variables
      (

       ;; create variable 'my-n'
       (my-n

        ;; if my-n is null
        (if (null my-n)

            ;; my-n is null: set it as 1
            1

          ;; my-n is not null: set it as my-n
          my-n))

       ;; create variable 'my-i'
        (my-i

         ;; set it as 1
         1))

    ;; while my-i is lower than my-n
    (while (<= my-i my-n)

      ;; if search backward for a previous text block
      (if (search-backward-regexp "\n[\t\n ]*\n+" nil "NOERROR")

          ;; found: go to beginnig of block text
          (progn

            ;; move point backward, stopping after a char not in STRING
            (skip-chars-backward "\t\n "))

        ;; did not found: go to beginning of buffer
        (progn

          ;; go to beginning of beffer
          (goto-char (point-min))

          ;; make my-n value the same as my-i
          (setq my-i my-n)))

      ;; add 1 to my-i
      (setq my-i (1+ my-i)))))


;;=========================================================
;; defun endless-fill-or-unfill
;;=========================================================
;; URL: http://endlessparentheses.com/fill-and-unfill-paragraphs-with-a-single-key.html?source=rss
(defun endless-fill-or-unfill ()
  "Like `fill-paragraph', but unfill if used twice."
  (interactive)
  ;; Make fill-column as big as the buffer, so it will unfill or the normal size
  (let ((fill-column
         ;; Simple way to make an toggle function, check if the last command was
         ;; this command
         (if (eq last-command 'endless-fill-or-unfill)
             ;; If true, make fill-column as big as it can be
             ;; remove this command from the history
             (progn (setq this-command nil)
                    (point-max))
           ;; If not, just set fill-column as default
           fill-column)))
    ;; Call fill-paragraph, because it uses fill-column
    (call-interactively #'fill-paragraph)))

;;=========================================================
;; defun xah-open-in-external-app
;;=========================================================
(defun xah-open-in-external-app ()
  "Open the current file or dired marked files in external app.
The app is chosen from your OS's preference.
URL `http://ergoemacs.org/emacs/emacs_dired_open_file_in_ext_apps.html'
Version 2016-10-15"
  (interactive)
  (let* (($file-list
          (if (string-equal major-mode "dired-mode")
              (dired-get-marked-files)
            (list (buffer-file-name))))
         ($do-it-p (if (<= (length $file-list) 5)
                       t
                     (y-or-n-p "Open more than 5 files? "))))
    (when $do-it-p
      (cond
       ((string-equal system-type "windows-nt")
        (mapc
         (lambda ($fpath)
           (w32-shell-execute "open" (replace-regexp-in-string "/" "\\" $fpath t t))) $file-list))
       ((string-equal system-type "darwin")
        (mapc
         (lambda ($fpath)
           (shell-command
            (concat "open " (shell-quote-argument $fpath))))  $file-list))
       ((string-equal system-type "gnu/linux")
        (mapc
         (lambda ($fpath) (let ((process-connection-type nil))
                            (start-process "" nil "xdg-open" $fpath))) $file-list))))))


;;=========================================================
;; defun xah-open-in-terminal
;;=========================================================
(defun xah-open-in-terminal ()
  "Open the current dir in a new terminal window.
URL `http://ergoemacs.org/emacs/emacs_dired_open_file_in_ext_apps.html'
Version 2015-12-10"
  (interactive)
  (cond
   ((string-equal system-type "windows-nt")
    (message "Microsoft Windows not supported. File a bug report or pull request."))
   ((string-equal system-type "darwin")
    (message "Mac not supported. File a bug report or pull request."))
   ((string-equal system-type "gnu/linux")
    (let ((process-connection-type nil))
      (start-process "" nil "x-terminal-emulator"
                     (concat "--working-directory=" default-directory) )))))

;;=========================================================
;; user--skim-buffer
;; url: https://pastebin.com/Uu072Yqj
;;=========================================================
(defun user--skim-buffer (lines seconds)
  "Scrolls the buffer `lines` every `seconds`, which allows for roughly skimming over a buffer."
  (interactive "nScroll lines: \nnEvery seconds: ")
  (while (not (= (point) (point-max)))
    (scroll-up-line lines)
    (end-of-line)
    (sit-for seconds)))

;;=========================================================
;; user--clean-buffer
;; url: https://pastebin.com/Uu072Yqj
;;=========================================================
(defun user--clean-buffer () "Cleans the buffer by re-indenting, changing tabs to spaces, and removing trailing whitespace."
  (interactive)

  ;; Remove whitespace from the ends of lines
  (delete-trailing-whitespace)

  ;; Replace more than 2 newlines with 2 newlines
  (save-excursion
    (replace-regexp "^\n\\{3,\\}" "\n\n" nil (point-min) (point-max)))

  ;; Turn tabs into spaces
  (untabify (point-min) (point-max)))

;;=========================================================
;; user--make-temp-file
;; url: https://pastebin.com/Uu072Yqj
;;=========================================================
(defun user--make-temp-file (name)
  "Creates a temporary file in the system temp directory, for various purposes."
  (interactive "sFile name:")
  (generate-new-buffer name)
  (switch-to-buffer name)
  (write-file (concat temporary-file-directory name)))


;;=========================================================
;;
;; url: https://github.com/lunaryorn/old-emacs-configuration/blob/master/lisp/lunaryorn-files.el
;;=========================================================
(defun lunaryorn-delete-file-and-buffer ()
  "Delete the current file and kill the buffer."
  (interactive)
  (let ((filename (buffer-file-name)))
    (cond
     ((not filename) (kill-buffer))
     ((vc-backend filename) (vc-delete-file filename))
     (t
      (delete-file filename)
      (kill-buffer)))))

(defun lunaryorn-rename-file-and-buffer ()
  "Rename the current file and buffer."
  (interactive)
  (let* ((filename (buffer-file-name))
         (old-name (if filename
                       (file-name-nondirectory filename)
                     (buffer-name)))
         (new-name (read-file-name "New name: " nil nil nil old-name)))
    (cond
     ((not (and filename (file-exists-p filename))) (rename-buffer new-name))
     ((vc-backend filename) (vc-rename-file filename new-name))
     (t
      (rename-file filename new-name 'force-overwrite)
      (set-visited-file-name new-name 'no-query 'along-with-file)))))


(defun lunaryorn-current-file ()
  "Gets the \"file\" of the current buffer.
The file is the buffer's file name, or the `default-directory' in
`dired-mode'."
  (if (derived-mode-p 'dired-mode)
      default-directory
    (buffer-file-name)))

;;=========================================================
;; Grep notes
;;=========================================================
(defun my-grep-notes (regex)
  "Use FIND-GREP in my notes directory."

  (interactive "sWhat to SEARCH for? ")
  (if (< (length regex) 3)
      (message "Too Short. Try Again!!")
    (progn
      (let* ((notes-dir "~/Dropbox/Notes/")
             (regex-treated (replace-regexp-in-string " " ".*" regex))
             (my-find-c (concat "find " notes-dir  " -type f -exec "))
             (my-grep-c (concat "grep --color -nH -i -e '" regex-treated "' {} +"))
             (command (concat my-find-c my-grep-c)))

        ;; find . -type f -exec grep --color -nH -e javascript {} +
        (grep-find command)
        (switch-to-buffer-other-frame "*grep*")))))

;;=========================================================
;; from a Gist by Michael Fogleman
;; https://gist.github.com/mwfogleman/95cc60c87a9323876c6c
;; even though I am pretty comfortable with the C-x n n / C-x n w
;; default keys settings
;;=========================================================
(defun narrow-or-widen-dwim (p)
  "If the buffer is narrowed, it widens. Otherwise, it narrows intelligently.
Intelligently means: region, subtree, or defun, whichever applies
first.
With prefix P, don't widen, just narrow even if buffer is already
narrowed."
  (interactive "P")
  (declare (interactive-only))
  (cond ((and (buffer-narrowed-p) (not p)) (widen))
        ((region-active-p)
         (narrow-to-region (region-beginning) (region-end)))
        ((derived-mode-p 'org-mode) (org-narrow-to-subtree))
        (t (narrow-to-defun))))

(provide 'init-customFunctions)
