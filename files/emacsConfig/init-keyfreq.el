;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Key Frequence
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package keyfreq
  :ensure t
  :config
  (progn
    (message "Keyfreq - Loaded")

    ;; Commands that are not listed in (keyfreq-show)
    ;; FIXME: add more commands that are not needed to be listed
    (setq keyfreq-excluded-commands
	  '(self-insert-command
	    abort-recursive-edit
	    forward-char
	    backward-char
	    previous-line
	    next-line))

    ;; Start keyfreq mode
    (keyfreq-mode 1)

    ;; Star key freq auto sabe
    (keyfreq-autosave-mode 1)))


(provide 'init-keyfreq)
