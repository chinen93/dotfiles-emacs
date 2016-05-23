;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Key Binding
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;; Unset Key binding F(num)
(global-unset-key (kbd "<f2>"))
(global-unset-key (kbd "<f3>"))
(global-unset-key (kbd "<f4>"))
(global-unset-key (kbd "<f5>"))
(global-unset-key (kbd "<f6>"))
(global-unset-key (kbd "<f7>"))
(global-unset-key (kbd "<f8>"))
(global-unset-key (kbd "<f9>"))
(global-unset-key (kbd "<f10>"))
(global-unset-key (kbd "<f11>"))
(global-unset-key (kbd "<f12>"))

;;;; Unset digit-arguments
(global-unset-key (kbd "C-1"))
(global-unset-key (kbd "C-2"))
(global-unset-key (kbd "C-3"))
(global-unset-key (kbd "C-4"))
(global-unset-key (kbd "C-5"))
(global-unset-key (kbd "C-6"))
(global-unset-key (kbd "C-7"))
(global-unset-key (kbd "C-8"))
(global-unset-key (kbd "C-9"))
(global-unset-key (kbd "C-0"))
(global-unset-key (kbd "C--"))
(global-unset-key (kbd "M-1"))
(global-unset-key (kbd "M-2"))
(global-unset-key (kbd "M-3"))
(global-unset-key (kbd "M-4"))
(global-unset-key (kbd "M-5"))
(global-unset-key (kbd "M-6"))
(global-unset-key (kbd "M-7"))
(global-unset-key (kbd "M-8"))
(global-unset-key (kbd "M-9"))
(global-unset-key (kbd "M-0"))
(global-unset-key (kbd "M--"))

;;;; Unset every single key chord: start with C-
(global-unset-key (kbd "C-q"))
(global-unset-key (kbd "C-w"))
(global-unset-key (kbd "C-e"))
(global-unset-key (kbd "C-r"))
(global-unset-key (kbd "C-t"))
;; (global-unset-key (kbd "C-u")) ;; digit-argument
;; (global-unset-key (kbd "C-i")) ;; In emacs C-i is the sama as TAB
(global-unset-key (kbd "C-o"))
(global-unset-key (kbd "C-p"))
(global-unset-key (kbd "C-a"))
;; (global-unset-key (kbd "C-s")) ;; isearch-regexp
(global-unset-key (kbd "C-d"))
(global-unset-key (kbd "C-f"))
;; (global-unset-key (kbd "C-g")) ;; keyboard-quit
(global-unset-key (kbd "C-h"))
(global-unset-key (kbd "C-j"))
(global-unset-key (kbd "C-k"))
;; (global-unset-key (kbd "C-l")) ;; recenter-top-botton
(global-unset-key (kbd "C-ç"))
(global-unset-key (kbd "C-z"))
;; (global-unset-key (kbd "C-x")) ;; prefix-command
(global-unset-key (kbd "C-c"))
;; (global-unset-key (kbd "C-v")) ;; prefix-command
(global-unset-key (kbd "C-b"))
(global-unset-key (kbd "C-n"))
;; (global-unset-key (kbd "C-m")) ;; in emacs C-m is the same as RET

;;;; Unset every single key chord: start with M-
(global-unset-key (kbd "M-q"))
;; (global-unset-key (kbd "M-w")) ;; kill-ring-save
(global-unset-key (kbd "M-e"))
(global-unset-key (kbd "M-r"))
(global-unset-key (kbd "M-t"))
(global-unset-key (kbd "M-y"))
(global-unset-key (kbd "M-u"))
(global-unset-key (kbd "M-i"))
(global-unset-key (kbd "M-o"))
(global-unset-key (kbd "M-p"))
(global-unset-key (kbd "M-a"))
(global-unset-key (kbd "M-s"))
(global-unset-key (kbd "M-d"))
(global-unset-key (kbd "M-f"))
(global-unset-key (kbd "M-g"))
(global-unset-key (kbd "M-h"))
(global-unset-key (kbd "M-j"))
(global-unset-key (kbd "M-k"))
(global-unset-key (kbd "M-l"))
(global-unset-key (kbd "M-z"))
;; (global-unset-key (kbd "M-x")) ;; execute-extended-command
(global-unset-key (kbd "M-c"))
;; (global-unset-key (kbd "M-v")) ;; prefix-command
(global-unset-key (kbd "M-b"))
(global-unset-key (kbd "M-n"))
(global-unset-key (kbd "M-m"))

;;;; set Control/Meta plus number
;; (global-set-key (kbd "C-1"))
;; (global-set-key (kbd "C-2"))
;; (global-set-key (kbd "C-3"))
;; (global-set-key (kbd "C-4"))
;; (global-set-key (kbd "C-5"))
;; (global-set-key (kbd "C-6"))
;; (global-set-key (kbd "C-7"))
;; (global-set-key (kbd "C-8"))
;; (global-set-key (kbd "C-9"))
;; (global-set-key (kbd "C-0"))
;; (global-set-key (kbd "C--"))
(global-set-key (kbd "M-1") 'xah-cut-line-or-region)
;; (global-set-key (kbd "M-2"))
(global-set-key (kbd "M-3") 'yank)
;; (global-set-key (kbd "M-4"))
;; (global-set-key (kbd "M-5"))
;; (global-set-key (kbd "M-6"))
;; (global-set-key (kbd "M-7"))
;; (global-set-key (kbd "M-8"))
;; (global-set-key (kbd "M-9"))
;; (global-set-key (kbd "M-0"))
;; (global-set-key (kbd "M--"))

;;;; Unset key for frequent mistyped press
(global-unset-key (kbd "C-x DEL"))
(global-unset-key (kbd "C-x f"))

;;;; Define new prefix command
(define-prefix-command 'my-prefix-command)
(global-set-key (kbd "C-v") 'my-prefix-command)
(global-set-key (kbd "M-v") 'my-prefix-command)

;;;; Key binding using my prefix command
(global-set-key (kbd "M-v M-s") 'save-buffer)
(global-set-key (kbd "M-v M-f") 'find-file)
(global-set-key (kbd "M-v TAB") 'list-buffers)

;;;; Key binding using super
(global-set-key (kbd "s-q") 'other-window)
(global-set-key (kbd "s-w") 'delete-window)

;;;; Key binding special keys
(global-set-key (kbd "<prior>") 'xah-backward-block)
(global-set-key (kbd "<next>") 'xah-forward-block)

;;;; Set every single key chord: start with C-
;; (global-set-key (kbd "C-q"))
;; (global-set-key (kbd "C-w"))
;; (global-set-key (kbd "C-e"))
(global-set-key (kbd "C-r") 'isearch-backward-regexp)
;; (global-set-key (kbd "C-t"))
;; (global-set-key (kbd "C-u")) ;; digit-argument
;; (global-set-key (kbd "C-i")) ;; In emacs C-i is the sama as TAB
;; (global-set-key (kbd "C-o"))
(global-set-key (kbd "C-p") 'my-trim-right)
;; (global-set-key (kbd "C-a"))
(global-set-key (kbd "C-s") 'isearch-forward-regexp) 
;; (global-set-key (kbd "C-d"))
;; (global-set-key (kbd "C-f"))
;; (global-set-key (kbd "C-g")) ;; keyboard-quit
;; (global-set-key (kbd "C-h"))
;; (global-set-key (kbd "C-j"))
;; (global-set-key (kbd "C-k"))
;; (global-set-key (kbd "C-l")) ;; recenter-top-botton
;; (global-set-key (kbd "C-ç"))
;; (global-set-key (kbd "C-z"))
;; (global-set-key (kbd "C-x")) ;; prefix-command
;; (global-set-key (kbd "C-c"))
;; (global-set-key (kbd "C-v")) ;; prefix-command
;; (global-set-key (kbd "C-b"))
(global-set-key (kbd "C-n") 'xah-new-empty-buffer)
;; (global-set-key (kbd "C-m")) ;; in emacs C-m is the same as RET

;;;; Set every single key chord: start with M-
;; (global-set-key (kbd "M-q"))
;; (global-set-key (kbd "M-w")) ;; kill-ring-save
;; (global-set-key (kbd "M-e"))
;; (global-set-key (kbd "M-r"))
;; (global-set-key (kbd "M-t"))
;; (global-set-key (kbd "M-y"))
;; (global-set-key (kbd "M-u"))
;; (global-set-key (kbd "M-i"))
;; (global-set-key (kbd "M-o"))
;; (global-set-key (kbd "M-p"))
;; (global-set-key (kbd "M-a"))
;; (global-set-key (kbd "M-s"))
;; (global-set-key (kbd "M-d"))
;; (global-set-key (kbd "M-f"))
;; (global-set-key (kbd "M-g"))
;; (global-set-key (kbd "M-h"))
;; (global-set-key (kbd "M-j"))
;; (global-set-key (kbd "M-k"))
;; (global-set-key (kbd "M-l"))
;; (global-set-key (kbd "M-z"))
;; (global-set-key (kbd "M-x")) ;; execute-extended-command
;; (global-set-key (kbd "M-c"))
;; (global-set-key (kbd "M-v")) ;; prefix-command
;; (global-set-key (kbd "M-b"))
;; (global-set-key (kbd "M-n"))
;; (global-set-key (kbd "M-m"))

(provide 'init-keyBinding)
