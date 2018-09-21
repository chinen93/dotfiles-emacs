;; Don't edit this file, edit ~/emacsConfig/init-basicConfig.org instead ...

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

;; Don't indent automatically. Go to the beginning of the newline
(electric-indent-mode -1)

;; Set encoding charset
(set-language-environment 'utf-8)

;; For old Carbon emacs on OS X only
(set-keyboard-coding-system 'utf-8-mac)
(setq locale-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(unless (eq system-type 'windows-nt)
  (set-selection-coding-system 'utf-8))
(prefer-coding-system 'utf-8)

;; Change (yes/no) to (y/n)
(fset 'yes-or-no-p 'y-or-n-p)

;; Set initial message for *scratch* buffer
(setq initial-scratch-message 
";;***************************************************************
;;*************************** SCRATCH ***************************
;;***************************************************************

;;| Copy & Paste                                                |
;;|-------------------------------------------------------------|
;;| M-1 : cut line                     | M-3 : paste clipboard  |

;;| New command prefix (C-v) and (M-v)                          |
;;|-------------------------------------------------------------|
;;| M-v M-f : find-file                                         |

;;| Useful keybindings                                          |
;;|------------------------------------+------------------------|
;;| C-r : backward regex               | C-s   : forward regex  |
;;| C-n : new buffer                   | C-TAB : other-window   |
;;| C-0 : undo                         | M-q   : Hydra Launcher |

;;| Open Hydra Launcher - More commands inside !!               |
;;|------------------------------------+------------------------|
;;| M-q   : Hydra Launcher                                      |

")

;; Don't follow version controlled files change it locally.
;; Git will know that the file has changed.
(setq vc-follow-symlinks nil)

;; Remove tool bar at top and scroll bar at right
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))


;; Don't show start up message
(setq inhibit-startup-message t)

;; Set Dropbox folder
(setq my-dropbox-folder "~/Dropbox")

;; Config for Linux Only
(unless (eq system-type 'windows-nt)

  ;; A GNU Emacs library to ensure environment variables inside Emacs
  ;; look the same as in the user's shell.
  (use-package exec-path-from-shell
    :ensure t
    :config (progn
              (exec-path-from-shell-initialize))))

;; Config for Windows Only
(when (eq system-type 'windows-nt)
  ;; Dropbox is in another folder
  (setq my-dropbox-folder
        (concat (substring (shell-command-to-string "ECHO %USERPROFILE%")
                           0
                           -1)
                "\\Dropbox")))

;; Confirm Emacs before exiting
(setq confirm-kill-emacs 'yes-or-no-p)

;; Delete selected text when something is inserted and a mark is active
(delete-selection-mode 1)

;; When yanking with mouse, don't move the point. Just yank it.
(setq mouse-yank-at-point t)

;; Used for formatting time values
(setq system-time-locale "C")

;; Don't delete file, but move to trash instead
(setq delete-by-moving-to-trash t)

(provide 'init-basicConfig)
