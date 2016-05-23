;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Bookmark
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq bookmark-default-file  "~/emacsBookmark.el")

(setq bookmark-save-flag 1)

;; (global-set-key (kbd "M-v M-p") 'bookmark-bmenu-list)
(global-set-key (kbd "M-v M-l") 'bookmark-set)

(provide 'init-bookmark)
