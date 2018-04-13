;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Ledger Package
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package ledger-mode
  :ensure t
  :config
  (progn
    (message "Ledger - Loaded")

    ;; Clear whole transactions, not individual postings.
    (setq ledger-clear-whole-transactions t)

    ;; awalker4 mode to clean the ledger buffer
    ;; https://github.com/awalker4/.dotfiles/blob/master/emacs.d/config.org
    (defun aw/clean-ledger-on-save ()
      (interactive)
      (if (eq major-mode 'ledger-mode)
          (let ((curr-line (line-number-at-pos)))
            (ledger-mode-clean-buffer)
            (line-move (- curr-line 1)))))

    (add-hook 'before-save-hook 'aw/clean-ledger-on-save)
    
    (eval-after-load 'ledger-mode
      (define-key ledger-mode-map (kbd "M-q") nil))))

(provide 'init-ledger-mode)
