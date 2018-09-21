;; Don't edit this file, edit ~/emacsConfig/init-bookmark.org instead ...

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Bookmark
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  ;; Set default file where bookmarks will be saved.
  (setq bookmark-default-file  "~/emacsBookmark.el")

  (file-exists-p "~/Dropbox")

  ;; Save bookmarks every time you make or delete a bookmark.
  (setq bookmark-save-flag 1)

  ;; Bookmarks are displayed in LIFO order
  (setq bookmark-sort-flag nil)

  ;; (global-set-key (kbd "M-v M-p") 'bookmark-bmenu-list)
  (global-set-key (kbd "M-v M-l") 'bookmark-set)

  (provide 'init-bookmark)
