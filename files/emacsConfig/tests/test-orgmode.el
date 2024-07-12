;; test-orgmode.el                    -*- lexical-binding: t; -*-

;; ============ VARIABLES ============

(defvar my-test/orgmode-filename
  "my-test-orgmode"
  "Orgmode filename for test purposes")

(defvar my-test/orgmode-header
  (concat "#+title:      Test \n"
          "#+identifier: 20240411T185646 \n\n"
          "* TODO HEADING 1\n"
	  ":PROPERTIES:\n"
	  ":Created:    2024-06-11\n"
	  ":END:\n\n"
          "* DONE HEADING 2\n"
	  "CLOSED: [2024-07-11 Thu 22:33]\n"
	  ":PROPERTIES:\n"
	  ":Created:    2024-06-11\n"
	  ":END:\n\n")
  "Orgmode information for test purposes")

;; ============ TESTS  ============

(ert-deftest my-test--org-get-closed-timestamp ()
  "Test if my--get-org-get-closed-timestamp works properly"
  :tags '(orgmode)

  (my-with-test-org-file my-test/orgmode-filename
			 my-test/orgmode-header
			 (beginning-of-buffer)
			 (search-forward "HEADING 1" nil nil nil)
			 (should (equal (my--org-get-closed-timestamp) "")))

  (my-with-test-org-file my-test/orgmode-filename
			 my-test/orgmode-header
			 (beginning-of-buffer)
			 (search-forward "HEADING 2" nil nil nil)
			 (should (equal (my--org-get-closed-timestamp) "[2024-07-11 Thu 22:33]"))))

;; ========================

(provide 'test-orgmode)
