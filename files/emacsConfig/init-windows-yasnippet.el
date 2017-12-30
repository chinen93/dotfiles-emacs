;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; YASNIPPET configuration
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require-package 'yasnippet)

;; Change add Directories when looking for snippets
(setq yas-snippet-dirs
      ;; Personal Collection
      '("~/emacsSnippets"))

(yas-global-mode)

(provide 'init-windows-yasnippet)
