;; Don't edit this file, edit ~/emacsConfig/init-fun.org instead ...

  (use-package goto-addr
    ;; http://xenodium.com/#actionable-urls-in-emacs-buffers
    :hook ((compilation-mode . goto-address-mode)
           (prog-mode . goto-address-prog-mode)
           (eshell-mode . goto-address-mode)
           (shell-mode . goto-address-mode))
    :bind (:map goto-address-highlight-keymap
                ("<RET>" . goto-address-at-point)
                ("M-<RET>" . newline))
    :commands (goto-address-prog-mode
               goto-address-mode))
(use-package keyfreq
    :ensure t)
  (when (featurep 'keyfreq)
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
    (keyfreq-autosave-mode 1))
  (use-package rainbow-delimiters
    :ensure t)
  (when (featurep 'rainbow-delimiters)
    (require 'rainbow-delimiters)
    (add-hook 'prog-mode-hook 'rainbow-delimiters-mode))
  (use-package define-word
    :ensure t)
  (use-package string-inflection
    :ensure t)
  (use-package nov
    :ensure t)
  (when (featurep 'nov)
    (setq nov-text-width most-positive-fixnum)
    (add-hook 'nov-mode-hook 'visual-line-mode)

    (setq nov-text-width 80)

    (defun my-nov-font-setup ()
      (face-remap-add-relative
       'variable-pitch
       :family "Liberation Serif"
       :height 1.0))

    (add-hook 'nov-mode-hook 'my-nov-font-setup)

    (add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode)))
  (use-package try
    :ensure t)
  (use-package re-builder
    :ensure t)

  (use-package multiple-cursors
    :ensure t)
  (when (featurep 'multiple-cursors)
    (global-set-key (kbd "C->") 'mc/mark-next-like-this)
    (global-set-key (kbd "C-<") 'mc/mark-previous-like-this))
    (use-package elmacro
 
      :ensure t)
  (when (featurep 'elmacro) 
    (elmacro-mode))
  (use-package flycheck
    :ensure t)
  (when (featurep 'flycheck)
    ;; Flycheck gets to be a bit much when warning about checkdoc issues.
    (setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc))

    (add-hook 'prog-mode-hook 'flycheck-mode))
  (use-package expand-region
    :ensure t)
  (when (featurep 'expand-region)
    (global-set-key (kbd "C-=") 'er/expand-region))
  (require-package 'company)
  ;; Set delay to start completition
  (setq company-idle-delay 0.2)
  (setq company-echo-delay 0)

  ;; Keep the return of company as-is
  (setq company-dabbrev-downcase nil)

  ;; Minimimum size to start to search for match
  (setq company-minimum-prefix-length 3)
  (setq company-selection-wrap-around t)

  ;; Sort matches by occurrence and backend importance
  (setq company-transformers '(company-sort-by-occurrence
                               company-sort-by-backend-importance))
  ;; Start mode globally
  (add-hook 'after-init-hook 'global-company-mode)
