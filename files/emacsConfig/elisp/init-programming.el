;; Don't edit this file, edit ~/emacsConfig/init-programming.org instead ...

  (defun infer-indentation-style ()
    ;; if our source file uses tabs, we use tabs, if spaces spaces, and if
    ;; neither, we use the current indent-tabs-mode
    (let ((space-count (how-many "^  " (point-min) (point-max)))
          (tab-count (how-many "^\t" (point-min) (point-max))))
      (if (> space-count tab-count) (setq indent-tabs-mode nil))
      (if (> tab-count space-count) (setq indent-tabs-mode t))))

  (add-hook 'prog-mode-hook 'infer-indentation-style)
  (use-package js2-mode
    :ensure t)
  (when (featurep 'js2-mode)
    ;; number of spaces when identing
    (setq indent-tabs-mode nil)
    (setq js2-basic-offset 2)

    ;; load this mode when loading .js files
    (add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
    (add-to-list 'auto-mode-alist '("\\.json\\'" . js2-mode)))
  (use-package markdown-mode
    :ensure t)
  (when (featurep 'markdown-mode)
    ;; :commands (markdown-mode gfm-mode)
    ;; :mode (("README\\.md\\'" . gfm-mode)
    ;;        ("\\.md\\'" . markdown-mode)
    ;;        ("\\.markdown\\'" . markdown-mode))
    ;; :init (setq markdown-command "multimarkdown"))
  )
  (use-package ggtags
    :ensure t)
  (when (featurep 'ggtags)
    (ggtags-mode 1)
    (add-hook 'c-mode-common-hook
              (lambda ()
                (when (derived-mode-p 'c-mode 'c++-mode 'java-mode 'asm-mode)
                  (ggtags-mode 1)))))
