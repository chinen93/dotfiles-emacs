;; helper-functions.el                    -*- lexical-binding: t; -*-

(defun my-test-create-file (filename &optional body)
  "Create file for tests on .emacs.d directory"

  (let* ((file (my--test-get-fullpath-filename filename))
         (buffer (find-file-noselect file 'nowarn))
         (body (or body "")))
    (with-current-buffer buffer
      (point-min)
      (insert body)
      (write-file file nil))))

(defun my-test-delete-file (filename filepath)
  "Delete file and buffer"

  (ignore-errors
    (kill-buffer filename)
    (delete-file filepath)))

;; ============ Private Functions ============

(defun my--test-get-fullpath-filename (filename)
  "Get fullpath filename"
  (concat my/config-emacs.d-folder "tests/" filename))

;; ========================

(provide 'helper-function)
