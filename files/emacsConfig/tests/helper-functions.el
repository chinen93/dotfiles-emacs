;; helper-functions.el                    -*- lexical-binding: t; -*-

;; ============ Buffer ============

(defmacro my-with-test-buffer (contents &rest body)
  "Execute BODY in a temporary buffer with CONTENTS."
  (declare (indent 1) (debug t))

  `(with-temp-buffer
     (insert ,contents)
     (goto-char (point-min))
     ,@body))

(defmacro my-with-test-org-buffer (contents &rest body)
  "Execute BODY in a temporary Org buffer containing CONTENTS."
  `(my-with-test-buffer ,contents
			(delay-mode-hooks
			  (org-mode))
			,@body))

;; ============ File ============

(defmacro my-with-test-file (contents &rest body)
  "Execute BODY in a temporary file containing CONTENTS."
  (declare (indent 1) (debug t))

  `(let* ((file (make-temp-file "my-test-" nil))
          (buffer (find-file-noselect file t)))
     (unwind-protect
         (with-current-buffer buffer
           (erase-buffer)
           (insert ,contents)
           (basic-save-buffer)
           (goto-char (point-min))
           ,@body)
       (when (buffer-live-p buffer)
         (set-buffer-modified-p nil)
         (kill-buffer buffer))
       (when (file-exists-p file)
         (delete-file file)))))

(defmacro my-with-test-org-file (contents &rest body)
  "Execute BODY in a temporary Org file containing CONTENTS."
  (declare (indent 1) (debug (form body)))

  `(my-with-test-file ,contents
     (delay-mode-hooks
       (org-mode))
     ,@body))

;; ============ Assert Functions ============

(defmacro should-equal (first second)
  "Macro to shorten (should (equal )) common command to test"
  `(should (equal ,first ,second)))

;; ========================

(provide 'helper-function)
