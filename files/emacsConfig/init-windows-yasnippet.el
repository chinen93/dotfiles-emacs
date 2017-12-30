;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; YASNIPPET configuration
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require-package 'yasnippet)


(yas-global-mode)

;; Change add Directories when looking for snippets
(setq yas-snippet-dirs
      ;; Personal Collection
      '("~/emacsSnippets"))

(provide 'init-windows-yasnippet)
