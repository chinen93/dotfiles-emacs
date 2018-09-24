;; Don't edit this file, edit ~/emacsConfig/init-ledger.org instead ...

  (use-package ledger-mode
    :ensure t)
  (when (featurep 'ledger-mode)
    (defvar my-ledger-file "~/Dropbox/Ledger.ledger"
      "Data file where ledger-cli search from.")

    ;; Set master file
    (setq ledger-master-file my-ledger-file)

    ;; Clear whole transactions, not individual postings.
    (setq ledger-clear-whole-transactions t)

    (defvar my-ledger-cmd (format "ledger -f %s"
                                  my-ledger-file)
      "Beginning of every command has to be this"))
  (when (featurep 'ledger-mode)
    ;; awalker4 mode to clean the ledger buffer
    ;; https://github.com/awalker4/.dotfiles/blob/master/emacs.d/config.org
    (defun aw/clean-ledger-on-save ()
      (interactive)
      (if (eq major-mode 'ledger-mode)
          (let ((curr-line (line-number-at-pos)))
            (ledger-mode-clean-buffer)
            (line-move (- curr-line 1)))))

    ;; Add Helper function to before save hook
    (add-hook 'before-save-hook 'aw/clean-ledger-on-save)

  )
  (when (featurep 'ledger-mode)
    ;; Clear ledger reports before adding some more
    (setq ledger-reports nil)

    ;; Add more reports
    (ledger-reports-add "Balance Assets"
                        (concat my-ledger-cmd
                                " "
                                "balance Assets:Bank"))

    (ledger-reports-add "Balance Expenses All"
                        (concat my-ledger-cmd
                                " "
                                "balance Expenses"))

    (ledger-reports-add "Balance Expenses This Month"
                        (concat my-ledger-cmd
                                " "
                                "balance --period 'this month' Expenses"))

    (ledger-reports-add "Balance Expenses Last Month"
                        (concat my-ledger-cmd
                                " "
                                "balance --period 'last month' Expenses"))

    (ledger-reports-add "Register This Month"
                        (concat my-ledger-cmd
                                " "
                                "register --period 'this month'"))

    (ledger-reports-add "Register All"
                        (concat my-ledger-cmd
                                " "
                                "register"))

    (ledger-reports-add "Balance All"
                        (concat my-ledger-cmd
                                " "
                                "balance"))

  )
  (when (featurep 'ledger-mode)
    (eval-after-load 'ledger-mode
      (define-key ledger-mode-map (kbd "M-q") nil)))
