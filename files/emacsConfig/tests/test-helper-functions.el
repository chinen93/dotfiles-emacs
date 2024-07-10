;; test-helper-functions.el                    -*- lexical-binding: t; -*-

(ert-deftest my-test--test-get-full-path-filename ()
  "Test get full path filename"
  :tags '(my-test-functions)

  (let* ((filename "filename")
         (filepath (my--test-get-fullpath-filename filename)))
    (should (file-name-absolute-p filepath))))

(ert-deftest my-test--test-create-file ()
  "Test Creation of Test File

  Delete test file after checking"
  :tags '(my-test-functions)

  (let* ((filename "filename")
         (filepath (my--test-get-fullpath-filename filename)))
    (unwind-protect
        (progn 
          (should (not (file-exists-p filepath)))
          (my-test-create-file filename)
          (should (file-exists-p filepath)))

      (my-test-delete-file filename filepath))

    (should (not (file-exists-p filepath)))))

(ert-deftest my-test--test-create-file--with-body ()
  "Test Creation of Test File

  Delete test file after checking"
  :tags '(my-test-functions)

  (let* ((filename "filename")
         (filepath (my--test-get-fullpath-filename filename))
         (body "TEST\nBODY"))

    (unwind-protect
        (progn
          (should (not (file-exists-p filepath)))
          (my-test-create-file filename body)
          (should (file-exists-p filepath))
          (with-current-buffer filename
            (should (string-match-p body (buffer-string)))))

      (my-test-delete-file filename filepath))

    (should (not (file-exists-p filepath)))))

;; ========================

(provide 'test-helper-function)
