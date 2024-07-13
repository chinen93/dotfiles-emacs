;; test-denote.el                    -*- lexical-binding: t; -*-

;; ============ VARIABLES ============


(defvar my-test/denote-filename
  "my-test-denote-filename"
  "Denote filename for test purposes")

(defvar my-test/denote-identifier-value
  "20240411T185646"
  "Denote identifier value for test purposes")

(defvar my-test/denote-description-value
  "Test - "
  "Denote description value for test purposes")

(defvar my-test/denote-identifier
  (concat "#+title:      Test \n"
          "#+identifier: 20240411T185646")
  "Denote information for test purposes")

;; ============ TESTS  ============

(ert-deftest my-test--denote-get-description--valid ()
  "Test if my-denote-get-description returns a valid description"
  :tags '(link-custom-id)

  (my-with-test-file my-test/denote-filename
                     my-test/denote-identifier
                     (should-eq (my--denote-get-description)
				my-test/denote-description-value)))

(ert-deftest my-test--denote-get-description--invalid ()
  "Test if my-denote-get-description returns a invalid description"
  :tags '(link-custom-id)

  (my-with-test-file my-test/denote-filename
                     nil
                     (should-eq (my--denote-get-description)
				" - ")))

(ert-deftest my-test--denote-get-identifier--valid ()
  "Test if my-denote-get-identifier returns a valid identifier"
  :tags '(link-custom-id)

  (my-with-test-file my-test/denote-filename
                     my-test/denote-identifier
                     (should-eq (my--denote-get-identifier)
				my-test/denote-identifier-value)))

(ert-deftest my-test--denote-get-identifier--invalid ()
  "Test if my-denote-get-identifier returns a invalid identifier"
  :tags '(link-custom-id)

  (my-with-test-file my-test/denote-filename
                     nil
                     (should-error (my--denote-get-identifier) :type 'args-out-of-range)))

(ert-deftest my-test--denote-link-format-custom-search--empty ()
  "Test formatting denote link for custom ID when empty"
  :tags '(link-custom-id)

  (let ((my/denote-link-information nil))
    (should-eq (my--denote-link-format-custom-search)
	       nil)))

(ert-deftest my-test--denote-link-format-custom-search--valid ()
  "Test formatting denote link for custom ID"
  :tags '(link-custom-id)

  (let* ((identifier my-test/denote-identifier-value)
         (custom-search "#TestFile")
         (title my-test/denote-description-value)
         (my/denote-link-information (list identifier custom-search title)))
    (should-eq (my--denote-link-format-custom-search)
               "[[denote:20240411T185646::#TestFile][Test - ]]")))

(ert-deftest my-test--denote-link-get-or-create-custom-id--create ()
  "Test if custom id is created and returned"
  :tags '(link-custom-id)

  (my-with-test-file my-test/denote-filename
		     my-test/denote-identifier
                     (my-denote-link-get-or-create-custom-id)
		     (save-buffer)
                     (should-eq (safe-length my/denote-link-information)3)
                     (should-eq (nth 0 my/denote-link-information) my-test/denote-identifier-value)
                     (should (string-prefix-p "#" (nth 1 my/denote-link-information)))
                     (should-eq (nth 2 my/denote-link-information) my-test/denote-description-value)))

;; ========================

(provide 'test-denote)
