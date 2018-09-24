;; Don't edit this file, edit ~/emacsConfig/init-yasnippert.org instead ...

  (require-package 'yasnippet)

  (message "Yasnippet - Loaded")

  ;; Surppress yasnippet backquote changes
  (setq warning-suppress-types '(yasnippet backquote-change))

  ;; Controls indenting applied to snippets.
  (setq yas-indent-line 'fixed)

  ;; Change add Directories when looking for snippets
  (setq yas-snippet-dirs
        ;; Personal Collection
        '("~/emacsSnippets"))

  ;; Enable yasnippet mode globally
  (yas-global-mode)
