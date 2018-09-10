;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Fun Packages
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (use-package wotd
;;   :ensure t
;;   :config
;;   (progn
;;     (message "Word of the day - Loaded")
;;     ))

;; (use-package typit
;;   :ensure t
;;   :config
;;   (progn
;;     (message "Typit - Loaded")
;;     ))

;; (use-package focus
;;   :ensure t
;;   :config
;;   (progn
;;     (message "Focus - Loaded")
;;     ))

;; (use-package google-translate
;;   :ensure t
;;   :config
;;   (progn
;;     (message "Google Translate - Loaded")

;;     (require 'google-translate-smooth-ui)

;;     (setq google-translate-translation-directions-alist
;;           '( ("en" . "pt") ("pt" . "en")))
;;     ))

;; (use-package google-this
;;   :ensure t
;;   :config
;;   (progn
;;     (message "Google This - Loaded")
;;     ))

(use-package goto-addr
  ;; http://xenodium.com/#actionable-urls-in-emacs-buffers
  :hook ((compilation-mode . goto-address-mode)
         (prog-mode . goto-address-prog-mode)
         (eshell-mode . goto-address-mode)
         (shell-mode . goto-address-mode))
  :bind (:map goto-address-highlight-keymap
              ("<RET>" . goto-address-at-point)
              ("M-<RET>" . newline))
  :commands (goto-address-prog-mode
             goto-address-mode))

(provide 'init-fun)
