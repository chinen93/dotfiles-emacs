;; test-helper-functions.el                    -*- lexical-binding: t; -*-

;;;; ============================================================================
;;;; my-with-test-buffer
;;;; ============================================================================

(ert-deftest my-test-with-test-buffer--contents ()
  "Buffer should contain the provided contents."
  (my-with-test-buffer
      "Hello World"
    (should (equal (buffer-string) "Hello World"))))

(ert-deftest my-test-with-test-buffer--point-at-beginning ()
  "Point should start at the beginning of the buffer."
  (my-with-test-buffer
      "Hello"
    (should (= (point) (point-min)))))

(ert-deftest my-test-with-test-buffer--fundamental-mode ()
  "Temporary buffers should default to Fundamental mode."
  (my-with-test-buffer
      ""
    (should (eq major-mode 'fundamental-mode))))

;;;; ============================================================================
;;;; my-with-test-org-buffer
;;;; ============================================================================

(ert-deftest my-test-with-test-org-buffer--major-mode ()
  "Temporary Org buffer should be in Org mode."
  (my-with-test-org-buffer
      "#+title: Test"
    (should (eq major-mode 'org-mode))))

(ert-deftest my-test-with-test-org-buffer--contents ()
  "Org buffer should contain inserted text."
  (my-with-test-org-buffer
      "#+title: Test"
    (should
     (equal (buffer-string)
            "#+title: Test"))))

;;;; ============================================================================
;;;; my-with-test-file
;;;; ============================================================================

(ert-deftest my-test-with-test-file--creates-file ()
  "Temporary file should exist during the body."
  (my-with-test-file
      "abc"
    (should buffer-file-name)
    (should (file-exists-p buffer-file-name))
    (should (equal (buffer-string) "abc"))))

(ert-deftest my-test-with-test-file--point-at-beginning ()
  "Point should be at beginning."
  (my-with-test-file
      "abc"
    (should (= (point) (point-min)))))

(ert-deftest my-test-with-test-file--cleanup ()
  "Temporary file should be removed afterwards."
  (let (filename)
    (my-with-test-file
        "abc"
      (setq filename buffer-file-name)
      (should (file-exists-p filename)))
    (should-not (file-exists-p filename))))

;;;; ============================================================================
;;;; my-with-test-org-file
;;;; ============================================================================

(ert-deftest my-test-with-test-org-file--major-mode ()
  "Temporary file should be in Org mode."
  (my-with-test-org-file
      "#+title: Test"
    (should (eq major-mode 'org-mode))))

(ert-deftest my-test-with-test-org-file--visited-file ()
  "Org test file should visit a file."
  (my-with-test-org-file
      "#+title: Test"
    (should buffer-file-name)
    (should (file-exists-p buffer-file-name))))

(ert-deftest my-test-with-test-org-file--org-parsing ()
  "Org APIs should work normally."
  (my-with-test-org-file
      "#+title: Test\n\n* Heading"
    (goto-char (point-min))
    (should
     (equal
      (cadar (org-collect-keywords '("TITLE")))
      "Test"))))

;;;; ============================================================================
;;;; should-equal
;;;; ============================================================================

(ert-deftest my-test-should-equal--equal ()
  "should-equal should succeed on equal values."
  (should-equal '(1 2 3) '(1 2 3)))

(ert-deftest my-test-should-equal--string ()
  "should-equal should compare strings using `equal'."
  (should-equal "abc" (concat "a" "bc")))

;; ========================

(provide 'test-helper-function)
