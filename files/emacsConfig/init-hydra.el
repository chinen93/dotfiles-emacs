;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Hydra
;;
;; DATE_CREATE: 2016-06-29
;; DATE_UPDATE: 2016-07-13
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package hydra
  :ensure t
  :config
  (progn

    ;;; Hydra Zoom BEGIN
    (defhydra hydra-zoom (:color pink)
      "
 ^Zoom^
-^----^-----------------------------------------------------
"
      ("=" text-scale-increase nil)
      ("+" text-scale-increase "in")
      ("-" text-scale-decrease "out")
      ("0" (text-scale-set 0) "remove")

      ("q" nil "quit" :color blue)
      ("l" hydra-launcher/body "return" :color blue))
    ;;; Hydra Zoom END


    ;;; Hydra launcher BEGIN
    (defhydra hydra-launcher (:color amaranth
                                     :hint nil)
      "
 ^Functions^       ^Other Menus^       ^Org^
-^---------^-------^-----------^-------^---^---------
 _f_: find file    _z_: zoom           _m_: agenda    
 _s_: sort lines   _b_: buffer         _c_: capture   
 _p_: trim right   _v_: bookmarks                     
 _w_: whitespace   _g_: magit status                  
 ^ ^               _d_: display                       
 ^ ^               _y_: YASnippet                     

"
      ;; commands to exec in actual buffer
      ("p" my-trim-right)
      ("s" sort-lines)
      ("w" whitespace-mode)

      ;; commands to exit hydra-launcher
      ("m" org-agenda :color blue)
      ("b" buffer-menu :color blue)
      ("c" org-capture :color blue)
      ("d" hydra-display/body :color blue)
      ("f" helm-find-files :color blue)
      ("g" magit-status :color blue)
      ("v" helm-bookmarks :color blue)
      ("z" hydra-zoom/body :color blue)
      ("y" yas-insert-snippet :color blue)

      ;; move around text
      ("<right>" forward-char)
      ("<left>" backward-char)
      ("<down>" next-line)
      ("<up>" previous-line)
      ("C-<SPC>" set-mark-command)
      ("<home>" move-beginning-of-line)
      ("<end>" move-end-of-line)
      ("<RET>" nil :color blue)
      ("<ESC>" nil :color blue)

      ("q" nil "cancel" :color blue))
    (global-set-key (kbd "M-q") 'hydra-launcher/body)
    ;;; Hydra launcher END


    ;;; Hydra display BEGIN
    (defhydra hydra-display (:color pink
                                    :hint nil)
      "
 ^Window^            ^Frame^
-^------^------------^-----^---------
 _o_: other          _M-o_: other
 _1_: delete others  _M-n_: make new
 _2_: split below
 _3_: split right
 _0_: delete this

"
      ;; window
      ("1" delete-other-windows)
      ("2" split-window-below)
      ("3" split-window-right)
      ("o" other-window)
      ("0" delete-window)

      ;; frame
      ("M-n" make-frame)
      ("M-o" other-frame)

      ("q" nil "quit" :color blue)
      ("l" hydra-launcher/body "return" :color blue))
    ;;; Hydra display END


    ;;; Hydra menu buffer BEGIN
    (defhydra hydra-buffer-menu (:color pink
                                        :hint nil)
      "
 ^Mark^             ^Unmark^           ^Actions^          ^Search^
-^----^-------------^------^-----------^-------^----------^------^---------
 _m_: mark          _u_: unmark        _x_: execute       _R_: re-isearch
 _s_: save          _U_: unmark up     _b_: bury          _I_: isearch
 _d_: delete        ^ ^                _g_: refresh       _O_: multi-occur
 _D_: delete up     ^ ^                _T_: files only: % -28`Buffer-menu-files-only
 _~_: modified
 
"
      ("m" Buffer-menu-mark)
      ("u" Buffer-menu-unmark)
      ("U" Buffer-menu-backup-unmark)
      ("d" Buffer-menu-delete)
      ("D" Buffer-menu-delete-backwards)
      ("s" Buffer-menu-save)
      ("~" Buffer-menu-not-modified)
      ("x" Buffer-menu-execute)
      ("b" Buffer-menu-bury)
      ("T" Buffer-menu-toggle-files-only)
      ("O" Buffer-menu-multi-occur :color blue)
      ("I" Buffer-menu-isearch-buffers :color blue)
      ("R" Buffer-menu-isearch-buffers-regexp :color blue)
      ("v" Buffer-menu-select "select" :color blue)
      ("o" Buffer-menu-other-window "other-window" :color blue)

      ("g" revert-buffer)

      ("c" nil "cancel")
      ("q" quit-window "quit" :color blue))
    (define-key Buffer-menu-mode-map "." 'hydra-buffer-menu/body)
    ;;; Hydra menu buffer END


    ;;; Hydra org BEGIN
    (defhydra hydra-org (:color amaranth
                                :hint nil)
      "
 ^Org mode^
-^--------^-----------
 _T_: set tag
 _d_: set deadline
 _s_: set schedule date
 _c_: capture
 _a_: archive
"
      ("T" org-set-tags-command :color blue)
      ("d" org-deadline :color blue)
      ("s" org-schedule :color blue)
      ("t" org-time-stamp-inactive :color blue)
      ("c" org-capture :color blue)
      ("a" org-archive-subtree :color blue)
      
      ;; move around text
      ("<right>" forward-char)
      ("<left>" backward-char)
      ("<down>" next-line)
      ("<up>" previous-line)
      ("<home>" move-beginning-of-line)
      ("<end>" move-end-of-line)
      ("<tab>" org-cycle)
      ("<RET>" nil :color blue)
      ("<ESC>" nil :color blue)

      ("q" nil "cancel" :color blue))
    (define-key org-mode-map (kbd "M-a") 'hydra-org/body)
    ;;; Hydra org END


    ;;; Hydra elisp BEGIN
    (defhydra hydra-elisp (:color amaranth
                                  :hint nil)
      "
 ^Lisp^
-^----^-----------
 _r_: eval region
 _R_: eval buffer

"
      ("r" eval-region)
      ("R" eval-buffer)

      ("q" nil "cancel" :color blue))
    (define-key lisp-mode-map (kbd "M-a") 'hydra-elisp/body)
    ;;; Hydra elisp END


    ))
(provide 'init-hydra)
