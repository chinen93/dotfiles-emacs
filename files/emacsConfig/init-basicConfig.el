;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Basic Configuration
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; See the matching pair of parentheses and others characters
(show-paren-mode t)

;; Remove every warning, bell or visual
(setq ring-bell-function 'ignore)

;; Show number of line and column
(line-number-mode 1)
(setq column-number-mode t)

;; Set encoding charset
(set-language-environment 'utf-8)
(set-keyboard-coding-system 'utf-8-mac) ; For old Carbon emacs on OS X only
(setq locale-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(unless (eq system-type 'windows-nt)
  (set-selection-coding-system 'utf-8))
(prefer-coding-system 'utf-8)

;; Change (yes/no) to (y/n)
(fset 'yes-or-no-p 'y-or-n-p)

;; Set initial message for *scratch* buffer
(setq initial-scratch-message "
;***********************************************
;******************* SCRATCH *******************
;***********************************************          
")

;; Check this if config is in Linux
(unless (not (eq system-type 'windows-nt))
  (setq initial-scratch-message (concat initial-scratch-message "\n (my-load-hydra-helm-windows)"))
  )

;; Don't follow version controlled files change it locally.
;; Git will know that the file has changed.
(setq vc-follow-symlinks nil)

;; Remove tool bar at top and scroll bar at right
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))


;; Don't show start up message
(setq inhibit-startup-message t)

;; Check this if config is in Linux
(unless (eq system-type 'windows-nt)
  ;; A GNU Emacs library to ensure environment variables inside Emacs look the same as in the user's shell.
  (use-package exec-path-from-shell
    :ensure t
    :config (progn
	      (exec-path-from-shell-initialize))))

;; Confirm Emacs before exiting
(setq confirm-kill-emacs 'yes-or-no-p)

;; Delete selected text when something is inserted and a mark is active
(delete-selection-mode 1)

;; When yanking with mouse, don't move the point. Just yank it.
(setq mouse-yank-at-point t) 

;; Used for formatting time values
(setq system-time-locale "C")

;; Define new prefix command
(define-prefix-command 'my-prefix-command)
(global-set-key (kbd "C-v") 'my-prefix-command)
(global-set-key (kbd "M-v") 'my-prefix-command)

(provide 'init-basicConfig)
