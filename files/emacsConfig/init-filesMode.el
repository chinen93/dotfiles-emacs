;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; File Modes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq auto-mode-alist
      (append
       ;; File name (within directory) starts with a dot.
       '((".bashrc" . shell-script-mode)
	 (".bash_aliases" . shell-script-mode)
	 (".bash_profile" . shell-script-mode)
	 (".screenrc" . shell-script-mode)

	 ;; css mode
	 (".scss" . css-mode)

	 ;; File name has no dot.
	 ("/[^\\./]*\\'" . fundamental-mode)

	 ;; File name ends in ‘.C’.
	 ("\\.C\\'" . c++-mode))
       auto-mode-alist))

(provide 'init-filesMode)




