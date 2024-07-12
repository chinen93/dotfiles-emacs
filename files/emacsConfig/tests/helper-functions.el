;; helper-functions.el                    -*- lexical-binding: t; -*-

(defun my-test-create-file (filename &optional body)
  "Create file for tests on .emacs.d directory"

  (let* ((file (my--test-get-fullpath-filename filename))
         (buffer (find-file-noselect file 'nowarn))
         (body (or body "")))
    (with-current-buffer buffer
      (org-mode)
      (point-min)
      (insert body)
      (write-file file nil))))

(defun my-test-delete-file (filename filepath)
  "Delete file and buffer"

  (ignore-errors
    (kill-buffer filename)
    (delete-file filepath)))

(defmacro my-with-test-file (filename body &rest rest)
  "Create FILENAME with BODY and then evaluate REST"
  `(let* ((buffer-name ,filename)
          (buffer (my-test-create-file buffer-name ,body))
          (filepath (my--test-get-fullpath-filename buffer-name)))
     (with-current-buffer buffer-name
       (unwind-protect
           (progn 
             ,@rest)
         (my-test-delete-file buffer-name filepath)
	 ))))

(defmacro my-with-test-org-file (filename body &rest rest)
  "Create Org Mode FILENAME with BODY and then evaluate REST"
  `(my-with-test-file ,filename
		      ,body
		      (org-mode)
		      ,@rest))

;; ============ Private Functions ============

(defun my--test-get-fullpath-filename (filename)
  "Get fullpath filename"
  (concat my/config-emacs.d-folder "tests/" filename))

;; ========================

(provide 'helper-function)
