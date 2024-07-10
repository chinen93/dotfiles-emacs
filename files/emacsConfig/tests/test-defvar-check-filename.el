;; test-defvar-check-filename.el                    -*- lexical-binding: t; -*-

(ert-deftest my-test--defvar-check-filename--invalid ()
  "Test if file name is an invalid file"
  :tags '(defvar-check-filename)

  (should-error (my--defvar-check-filename "invalid-filename")))

(ert-deftest my-test--defvar-check-filename--valid ()
  "Test if file name is a valid file"
  :tags '(defvar-check-filename)

  (let* ((buffer-name "defvar-check-filename")
         (buffer (my-test-create-file buffer-name))
         (filepath (my--test-get-fullpath-filename buffer-name)))
    (unwind-protect
        (should (my--defvar-check-filename filepath))
      (my-test-delete-file buffer-name filepath))))

;; ========================

(provide 'test-defvar-check-filename)
