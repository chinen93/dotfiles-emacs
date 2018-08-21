;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Key Frequence
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package keyfreq
  :defer 2
  :ensure t
  :config
  (progn
    (message "Keyfreq - Loaded")

    ;; Commands that are not listed in (keyfreq-show)
    (setq keyfreq-excluded-commands
          '(self-insert-command
            abort-recursive-edit
            backward-char
            backward-delete-char-untabify
            c-electric-backspace
            company-ignore
            delete-backward-char
            forward-char
            helm-next-line
            helm-previous-line
            left-char
            mouse-drag-region
            mouse-set-point
            mwheel-scroll
            next-line
            org-delete-backward-char
            org-self-insert-command
            previous-line
            right-char))

    ;; Start keyfreq mode
    (keyfreq-mode 1)

    ;; Star key freq auto sabex
    (keyfreq-autosave-mode 1)))


(provide 'init-keyfreq)
