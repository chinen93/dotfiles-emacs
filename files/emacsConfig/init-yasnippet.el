;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; YASnippet Package
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package yasnippet
  :ensure t
  :config
  (progn
    (message "Yasnippet - Loaded")
    ;; Change add Directories when looking for snippets
    (setq yas-snippet-dirs
	  ;; Personal Collection
	  '("~/.snippets"))

    ;; Undefine default keys binding
    (define-key yas-minor-mode-map (kbd "<tab>") nil)
    (define-key yas-minor-mode-map (kbd "TAB") nil)

    ;; Create new key binding
    (define-key yas-minor-mode-map (kbd "<C-tab>") 'yas-expand)
    (define-key yas-minor-mode-map (kbd "C-v s") 'yas-insert-snippet)

    ;; Enable yasnippet mode globally
    (yas-global-mode)
    ))

(defun shk-yas/helm-prompt (prompt choices &optional display-fn)
  "Use helm to select a snippet. Put this into `yas-prompt-functions.'"
  (interactive)
  (setq display-fn (or display-fn 'identity))
  (if (require 'helm-config)
      (let (tmpsource cands result rmap)
	(setq cands (mapcar (lambda (x) (funcall display-fn x)) choices))
	(setq rmap (mapcar (lambda (x) (cons (funcall display-fn x) x)) choices))
	(setq tmpsource
	      (list
	       (cons 'name prompt)
	       (cons 'candidates cands)
	       '(action . (("Expand" . (lambda (selection) selection))))
	       ))
	(setq result (helm-other-buffer '(tmpsource) "*helm-select-yasnippet"))
	(if (null result)
	    (signal 'quit "user quit!")
	  (cdr (assoc result rmap))))
    nil))

(setq yas-prompt-functions '(shk-yas/helm-prompt))

(provide 'init-yasnippet)




