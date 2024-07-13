;; test-orgmode.el                    -*- lexical-binding: t; -*-

;; ============ VARIABLES ============

;; ==== Heading 1 ====

(defvar my-test/orgmode-heading-1-title
  "HEADING 1"
  "Orgmode heading 1 title for test purposes")

(defvar my-test/orgmode-heading-1-id
  "customID-b95a98b8-481d-42c1-b594-0775c1556ca2"
  "Orgmode heading 3 id for test purposes")

(defvar my-test/orgmode-heading-1-body
  (concat "* TODO " my-test/orgmode-heading-1-title "\n"
	  ":PROPERTIES:\n"
	  ":Created:    2024-06-11\n"
	  ":CUSTOM_ID: " my-test/orgmode-heading-1-id "\n"
	  ":END:\n\n")
  "Orgmode heading 1 for test purposes")

(defvar my-test/orgmode-heading-1-plan-update-string
  (concat "-  TODO "
	  "["
	  "[denote:20240411T185646::#customID-b95a98b8-481d-42c1-b594-0775c1556ca2]"
	  "[Test - HEADING 1]"
	  "]")
  "Orgmode heading 1 plan update string for test purposes")

;; ==== Heading 2 ====

(defvar my-test/orgmode-heading-2-title
  "HEADING 2"
  "Orgmode heading 2 title for test purposes")

(defvar my-test/orgmode-heading-2-closed-timestamp
  "[2024-07-11 Thu 22:33]"
  "Orgmode heading 2 id for test purposes")

(defvar my-test/orgmode-heading-2-body
  (concat "* DONE " my-test/orgmode-heading-2-title "\n"
	  "CLOSED: " my-test/orgmode-heading-2-closed-timestamp "\n"
	  ":PROPERTIES:\n"
	  ":Created:    2024-06-11\n"
	  ":END:\n\n")
  "Orgmode heading 2 for test purposes")

;; ==== Heading 3 ====

(defvar my-test/orgmode-heading-3-title
  "HEADING 3"
  "Orgmode heading 3 title for test purposes")

(defvar my-test/orgmode-heading-3-id
  "9954ef68-773b-4607-9b53-7231ed6e5928"
  "Orgmode heading 3 id for test purposes")

(defvar my-test/orgmode-heading-3-body
  (concat "* " my-test/orgmode-heading-3-title "\n"
	  ":PROPERTIES:\n"
	  ":Created:    2024-06-11\n"
	  ":ID:    " my-test/orgmode-heading-3-id "\n"
	  ":END:\n\n")
  "Orgmode heading 2 for test purposes")

;; ==== Others ====

(defvar my-test/orgmode-filename
  "my-test-orgmode"
  "Orgmode filename for test purposes")

(defvar my-test/orgmode-header
  (concat "#+title:      Test \n"
          "#+identifier: 20240411T185646 \n\n"
	  my-test/orgmode-heading-1-body
	  my-test/orgmode-heading-2-body
	  my-test/orgmode-heading-3-body)
  "Orgmode information for test purposes")

;; ============ TESTS  ============

(ert-deftest my-test--org-get-closed-timestamp ()
  "Test if my--get-org-get-closed-timestamp works properly"
  :tags '(orgmode)

  (my-with-test-org-file my-test/orgmode-filename
			 my-test/orgmode-header
			 (beginning-of-buffer)
			 (search-forward my-test/orgmode-heading-1-title)
			 (should-eq (my--org-get-closed-timestamp) ""))

  (my-with-test-org-file my-test/orgmode-filename
			 my-test/orgmode-header
			 (beginning-of-buffer)
			 (search-forward my-test/orgmode-heading-2-title)
			 (should-eq (my--org-get-closed-timestamp)
				    my-test/orgmode-heading-2-closed-timestamp)))


(ert-deftest my-test--org-get-plan-update-string ()
  "Test if my-org-get-plan-update-string works properly"
  :tags '(orgmode)

  (my-with-test-org-file my-test/orgmode-filename
			 my-test/orgmode-header
			 (beginning-of-buffer)
			 (search-forward my-test/orgmode-heading-1-title)
			 (my-org-get-plan-update-string)
			 (save-buffer)
			 (should-eq my/project-update
				    my-test/orgmode-heading-1-plan-update-string)))


(ert-deftest my-test--org-custom-id-generate-id ()
  "Test if my--org-custom-id-generate-id works properly"
  :tags '(orgmode)

  (my-with-test-org-file my-test/orgmode-filename
			 my-test/orgmode-header
			 (beginning-of-buffer)
			 (search-forward my-test/orgmode-heading-2-title)
			 (should (string-prefix-p "customID" (my--org-custom-id-generate-id)))
			 (save-buffer))

  (my-with-test-org-file my-test/orgmode-filename
			 my-test/orgmode-header
			 (beginning-of-buffer)
			 (search-forward my-test/orgmode-heading-3-title)
			 (should-eq (my--org-custom-id-generate-id)
				    (concat "customID-"	my-test/orgmode-heading-3-id))
			 (save-buffer)))


(ert-deftest my-test--org-custom-id-get-or-create ()
  "Test if my--org-custom-id-get-or-create works properly"
  :tags '(orgmode)

  (my-with-test-org-file my-test/orgmode-filename
			 my-test/orgmode-header
			 (beginning-of-buffer)
			 (search-forward my-test/orgmode-heading-2-title)
			 
			 (should (string-prefix-p "customID" (my-org-custom-id-get-or-create)))
			 (save-buffer))

  (my-with-test-org-file my-test/orgmode-filename
			 my-test/orgmode-header
			 (beginning-of-buffer)
			 (search-forward my-test/orgmode-heading-3-title)
			 (should-eq (my-org-custom-id-get-or-create)
				    (concat "customID-"	my-test/orgmode-heading-3-id))
			 (save-buffer)))


(ert-deftest my-test--org-custom-id-get-headlines ()
  "Test if my--org-custom-id-get-headlines works properly"
  :tags '(orgmode)

  (my-with-test-org-file my-test/orgmode-filename
			 my-test/orgmode-header
			 (beginning-of-buffer)
			 (search-forward my-test/orgmode-heading-1-title)
			 (org-narrow-to-subtree)
			 (should-eq (my--org-custom-id-get-headlines)
				    `((,my-test/orgmode-heading-1-title
				       (:title . ,my-test/orgmode-heading-1-title)
				       (:id)
				       (:custom-id . ,my-test/orgmode-heading-1-id))))))


(ert-deftest my-test--org-custom-id-parse-link-location ()
  "Test if my--org-custom-id-parse-link-location works properly"
  :tags '(orgmode)

  (should-eq (my--org-custom-id-parse-link-location `(,my-test/orgmode-heading-1-title
						      (:title . ,my-test/orgmode-heading-1-title)
						      (:id)
						      (:custom-id . ,my-test/orgmode-heading-1-id)))
	     (concat "#" my-test/orgmode-heading-1-id)))


;; ========================

(provide 'test-orgmode)
