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

;; Change (yes/no) to (y/n)
(fset 'yes-or-no-p 'y-or-n-p)

;; Set initial message for *scratch* buffer
(setq initial-scratch-message "
;***********************************************
;******************* SCRATCH *******************
;***********************************************          
")

;; Follow version controlled files without ask
(setq vc-follow-symlinks t)

;; Remove tool bar at top and scroll bar at right
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))


;; Don't show start up message
(setq inhibit-startup-message t)

;; A GNU Emacs library to ensure environment variables inside Emacs look the same as in the user's shell.
(use-package exec-path-from-shell
  :ensure t
  :config (progn
	    (exec-path-from-shell-initialize)))


;; Define new prefix command
(define-prefix-command 'my-prefix-command)
(global-set-key (kbd "C-v") 'my-prefix-command)
(global-set-key (kbd "M-v") 'my-prefix-command)

(provide 'init-basicConfig)
