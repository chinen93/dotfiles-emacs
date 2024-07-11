;; test-defvar-check-filename.el                    -*- lexical-binding: t; -*-

(ert-deftest my-test--defvar-check-filename--invalid ()
  "Test if file name is an invalid file"
  :tags '(defvar-check-filename)

  (should-error (my--defvar-check-filename "invalid-filename")))

(ert-deftest my-test--defvar-check-filename--valid ()
  "Test if file name is a valid file"
  :tags '(defvar-check-filename)

  (my-with-test-file "defvar-check-filename"
		     nil
		     (should (my--defvar-check-filename filepath))))

;; ========================

(provide 'test-defvar-check-filename)
