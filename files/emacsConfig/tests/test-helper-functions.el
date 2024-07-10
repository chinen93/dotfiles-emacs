;; test-helper-functions.el                    -*- lexical-binding: t; -*-

;; Test things that *should* happen.
(ert-deftest bbbbb-addition-test ()
  (should (= (+ 1 2) 3)))

;; Test things that your library *should trigger errors* for.
(ert-deftest bbbbb-div-by-0-test ()
  (should-error (/ 0 0)
                :type 'arith-error))


(provide 'test-helper-function)
