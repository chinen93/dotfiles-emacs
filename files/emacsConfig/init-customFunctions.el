;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Custom Functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; defun my-load-hydra-helm-windows BEGIN
(defun my-load-hydra-helm-windows ()
  "Load hydra and helm configurations properly on Windows"
  (interactive)

  (load-file "~/.emacsConfig/init-hydra.el")
  (load-file "~/.emacsConfig/init-helm.el"))
;; defun my-load-hydra-helm-windows END


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


;; ===================================================================
;; Functions from Internet
;; ===================================================================

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
;; defun xah-new-empty-buffer END


;; defun xah-forward-block BEGIN
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
;; defun xah-forward-block END
                    

;; defun xah-backward-block BEGIN
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
;; defun xah-backward-block END


;; defun endless-fill-or-unfill END
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
;; defun endless-fill-or-unfill END

(provide 'init-customFunctions)
