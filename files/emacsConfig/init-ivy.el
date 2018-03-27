;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; IVY configuration
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require-package 'ivy)

(ivy-mode 1)
;; add ‘recentf-mode’ and bookmarks to ‘ivy-switch-buffer’.
(setq ivy-use-virtual-buffers t)
;; number of result lines to display
(setq ivy-height 10)
;; does not count candidates
(setq ivy-count-format "")
;; no regexp by default
(setq ivy-initial-inputs-alist nil)
;; configure regexp engine.
(setq ivy-re-builders-alist
      ;; allow input not in order
      '((t   . ivy--regex-ignore-order)))

(global-set-key (kbd "C-s") 'swiper)
(global-set-key (kbd "M-x") 'counsel-M-x)

(global-set-key (kbd "M-v M-f") 'counsel-find-file)

(provide 'init-ivy)
