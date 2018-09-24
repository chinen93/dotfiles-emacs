;; Don't edit this file, edit ~/emacsConfig/init-modeline.org instead ...

  ;; Show number of line and column
  (line-number-mode 1)
  (setq column-number-mode t)
  (use-package diminish
    :ensure t)
  (when (featurep 'diminish)
    (diminish 'ivy-mode)
    (diminish 'company-mode)
    (diminish 'elmacro-mode)
    (diminish 'yas-global-mode)
    (diminish 'yas-minor-mode)
    (diminish 'flycheck-mode " FC")
    (diminish 'auto-revert-mode)

    (diminish 'helm))
  (use-package nyan-mode
    :ensure t)
  (when (featurep 'nyan-mode)
    ;; Max length of the nyan rainbow trail
    (setq nyan-bar-length 10)

    ;; start nyan mode
    (nyan-mode 1))
