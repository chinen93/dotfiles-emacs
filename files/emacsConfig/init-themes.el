;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Themes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Load theme
(use-package zenburn-theme 
  :defer 1
  :ensure t)

(use-package monokai-theme
  :defer 1
  :ensure t
  :config
  (progn
    (message "Monokai Theme - Loaded")))

(defun sakshamsharma-setTheme (themeName)
  "Set the theme to THEMENAME."
  (interactive "sWhat theme do you want to use? ")
  (when (display-graphic-p)
    (load-theme (intern themeName) t)))

(defun sakshamsharma-setFont (fntName)
  "Set the font to FNTNAME."
  (interactive "sWhat font name do you want to set? ")
  (set-face-attribute 'default nil
                      :family fntName
                      :height 105
                      :weight 'normal
                      :width 'normal))

(defun sakshamsharma-frameActions ()
  "Do actions to set up appearance of frame."
  (interactive)
  (let ((myTheme "monokai") (myFont "DejaVu Sans Mono"))
    ;; (disableBells)
    (sakshamsharma-setTheme myTheme)
    (sakshamsharma-setFont myFont)))

(sakshamsharma-frameActions)

(provide 'init-themes)
