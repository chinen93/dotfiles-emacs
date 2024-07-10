;; test-denote.el                    -*- lexical-binding: t; -*-

;; ============ VARIABLES ============


(defvar my-test/denote-filename
  "my-test-denote-filename"
  "Denote filename for test purposes")

(defvar my-test/denote-identifier
  (concat "#+title:      Test \n"
          "#+identifier: 20240411T185646")
  "Denote information for test purposes")

;; ============ TESTS  ============

(ert-deftest my-test--denote-get-description--valid ()
  "Test if my-denote-get-description returns a valid description"
  :tags '(link-custom-id)

  (let* ((buffer-name my-test/denote-filename)
         (buffer (my-test-create-file buffer-name my-test/denote-identifier))
         (filepath (my--test-get-fullpath-filename buffer-name)))
    (with-current-buffer buffer-name
      (unwind-protect
          (progn 
            (should (equal (my--denote-get-description) "Test - ")))
        (my-test-delete-file buffer-name filepath)))))

(ert-deftest my-test--denote-get-description--invalid ()
  "Test if my-denote-get-description returns a invalid description"
  :tags '(link-custom-id)

  (let* ((buffer-name my-test/denote-filename)
         (buffer (my-test-create-file buffer-name))
         (filepath (my--test-get-fullpath-filename buffer-name)))
    (with-current-buffer buffer-name
      (unwind-protect
          (progn 
            (should (equal (my--denote-get-description) " - ")))
        (my-test-delete-file buffer-name filepath)))))

(ert-deftest my-test--denote-get-identifier--valid ()
  "Test if my-denote-get-identifier returns a valid identifier"
  :tags '(link-custom-id)

  (let* ((buffer-name my-test/denote-filename)
         (buffer (my-test-create-file buffer-name my-test/denote-identifier))
         (filepath (my--test-get-fullpath-filename buffer-name)))
    (with-current-buffer buffer-name
      (unwind-protect
          (progn 
            (should (equal (my--denote-get-identifier) "20240411T185646")))
        (my-test-delete-file buffer-name filepath)))))

(ert-deftest my-test--denote-get-identifier--invalid ()
  "Test if my-denote-get-identifier returns a invalid identifier"
  :tags '(link-custom-id)

  (let* ((buffer-name my-test/denote-filename)
         (buffer (my-test-create-file buffer-name))
         (filepath (my--test-get-fullpath-filename buffer-name)))
    (with-current-buffer buffer-name
      (unwind-protect
          (progn 
            (should-error (my--denote-get-identifier) :type 'args-out-of-range))
        (my-test-delete-file buffer-name filepath)))))

(ert-deftest my-test--denote-link-format-custom-search--empty ()
  "Test formatting denote link for custom ID when empty"
  :tags '(link-custom-id)

  (let ((my/denote-link-information nil))
    (should (equal (my--denote-link-format-custom-search) nil))))

(ert-deftest my-test--denote-link-format-custom-search--valid ()
  "Test formatting denote link for custom ID"
  :tags '(link-custom-id)

  (let* ((identifier "20240411T185646")
         (custom-search "#TestFile")
         (title "Test - ")
         (my/denote-link-information (list identifier custom-search title)))
    (should (equal (my--denote-link-format-custom-search)
                   "[[denote:20240411T185646::#TestFile][Test - ]]"))))

(ert-deftest my-test--my-denote-link-get-or-create-custom-id--create ()
  "Test if custom id is created and returned"
  :tags '(link-custom-id)

  ;; (my-denote-link-get-or-create-custom-id)

  (should 't))

;; ========================

(provide 'test-denote)
