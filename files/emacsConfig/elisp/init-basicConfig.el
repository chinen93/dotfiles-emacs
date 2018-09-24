;; Don't edit this file, edit ~/emacsConfig/init-basicConfig.org instead ...

  ;; Set initial message for *scratch* buffer
  (setq initial-scratch-message "
  ;; ***************************************************************
  ;; *************************** SCRATCH ***************************
  ;; ***************************************************************

  ;; | Copy & Paste                                                |
  ;; |-------------------------------------------------------------|
  ;; | M-1 : cut line                     | M-3 : paste clipboard  |

  ;; | New command prefix (C-v) and (M-v)                          |
  ;; |-------------------------------------------------------------|
  ;; | M-v M-f : find-file                                         |

  ;; | Useful keybindings                                          |
  ;; |------------------------------------+------------------------|
  ;; | C-r : backward regex               | C-s   : forward regex  |
  ;; | C-n : new buffer                   | C-TAB : other-window   |
  ;; | C-0 : undo                         | M-q   : Hydra Launcher |

  ;; | Open Hydra Launcher - More commands inside !!               |
  ;; |------------------------------------+------------------------|
  ;; | M-q   : Hydra Launcher                                      |

  ")
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
  ;; See the matching pair of parentheses and others characters
  (show-paren-mode t)

  ;; Remove every warning, bell or visual
  (setq ring-bell-function 'ignore)

  ;; Remove tool bar at top and scroll bar at right
  (if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
  (if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

  ;; Don't show start up message
  (setq inhibit-startup-message t)
  ;; Change (yes/no) to (y/n)
  (fset 'yes-or-no-p 'y-or-n-p)
  ;; Confirm Emacs before exiting
  (setq confirm-kill-emacs 'yes-or-no-p)
  ;; Don't indent automatically. Go to the beginning of the newline
  (electric-indent-mode -1)
  ;; When yanking with mouse, don't move the point. Just yank it.
  (setq mouse-yank-at-point t)
  ;; Delete selected text when something is inserted and a mark is active
  (delete-selection-mode 1)

  ;; Don't delete file, but move to trash instead
  (setq delete-by-moving-to-trash t)
  ;; Don't follow version controlled files change it locally.
  ;; Git will know that the file has changed.
  (setq vc-follow-symlinks nil)
  ;; Set Dropbox folder
  (setq my-dropbox-folder "~/Dropbox")

  ;; Config for Windows Only
  (when (eq system-type 'windows-nt)
    ;; Dropbox is in another folder
    (setq my-dropbox-folder
          (concat (substring (shell-command-to-string "ECHO %USERPROFILE%")
                             0
                             -1)
                  "\\Dropbox")))
  (setq auto-mode-alist
        (append
         ;; File name (within directory) starts with a dot.
         '((".bashrc" . shell-script-mode)
           (".bash_aliases" . shell-script-mode)
           (".bash_profile" . shell-script-mode)
           (".screenrc" . shell-script-mode)
           (".ledgerrc" . shell-script-mode)

           ;; css mode
           (".scss" . css-mode)

           ;; File name has no dot.
           ("/[^\\./]*\\'" . fundamental-mode)

           ;; File name ends in ‘.C’.
           ("\\.C\\'" . c++-mode))
         auto-mode-alist))
  ;; Set directory to hold history
  (setq savehist-file "~/.emacs.d/savehist")

  ;; Start mode
  (savehist-mode 1)

  ;; FIXME
  (setq history-length t)

  ;; Delete duplicated history
  (setq history-delete-duplicates t)

  ;; Save minibuffer history
  (setq savehist-save-minibuffer-history 1)

  ;; Save hist for kill rings, search rings and regex search rings
  (setq savehist-additional-variables
        '(kill-ring
          search-ring
          regexp-search-ring))
  (require 'server)
  (unless (server-running-p)
    (server-start))
  ;; -a : show all entries even those "hidden".
  ;; -l : use a long listing format.
  ;; -H : follow symbolic links.
  ;; --group-directories-first : directory before files.
  (setq dired-listing-switches "-alH --group-directories-first")

  (defun xah-dired-mode-setup ()
    "to be run as hook for `dired-mode'."
    (dired-hide-details-mode 1))
  (add-hook 'dired-mode-hook 'xah-dired-mode-setup)
