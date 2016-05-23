;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; History
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

(provide 'init-history)
