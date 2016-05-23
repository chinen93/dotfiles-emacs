;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Custom Functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; defun my-bcompile-pchinen.el BEGIN
(defun my-bcompile-pchinen.el ()
  "Compile the pchinen.el to initializate faster"
  (interactive)

  ;; Compile pchinen.el so emacs starts up faster
  (byte-compile-file "/home/pchinen/git/dotfiles/files/pchinen.el"))
;; defun my-bcompile-pchinen.el END


;; defun my-open-initial-files BEGIN
(defun my-open-initial-files ()
  "Open some files in the initialization. Can be replaced by bookmarks"
  (interactive)

  ;; help file exist?: open it
  (if (file-exists-p "~/git/org/help.org")
      (find-file "~/git/org/help.org"))

  ;; pchinen.org exist?: open it
  (if (file-exists-p "~/.pchinen.org")
      (find-file "~/.pchinen.org"))

  ;; Vulcanet User
  (if (equal (user-login-name) "pedro")

      ;; Vulcanet notes exist?: open it
      (if (file-exists-p "~/vulcanet.org")
          (find-file "~/vulcanet.org"))))
;; defun my-open-initial-files END


;; defun my-find-function BEGIN
(defun my-find-function ()
  "Find every function in actual file with helm-swoop"
  (interactive)

  ;; set syntax for python
  (setq-local python-function-syntax "\\(#\\|def\\)")

  ;; Concatenate every function syntax
  (setq-local function-syntax (concat python-function-syntax))

  (helm-swoop :$query function-syntax))
;; defun my-find-function END


;; defun my-trim-right BEGIN
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
;; defun my-trim-right END


;; defun xah-cut-line-or-region BEGIN
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
;; defun xah-cut-line-or-region END


;; defun xah-new-empty-buffer BEGIN
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
;; defun xah-new-empty-buffer END


;; defun xah-forward-block BEGIN
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
;; defun xah-forward-block END
                    

;; defun xah-backward-block BEGIN
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
;; defun xah-backward-block END

(provide 'init-customFunctions)
