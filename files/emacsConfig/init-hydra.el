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
 ^Functions^         ^Other Menus^       ^Org^
-^---------^---------^-----------^-------^---^---------
 _0_: delete window  _z_: zoom           _m_: agenda    
 _1_: only 1 window  _b_: buffer         _c_: capture
 _2_: divide horiz   _v_: bookmarks                  
 _3_: divide vertc   _g_: magit status               
 _s_: sort lines     _d_: display                        
 _p_: trim right     _y_: YASnippet                      
 _w_: whitespace        
 ^ ^                    
 ^ ^                    

"
      ;; commands to exec in actual buffer
      ("p" my-trim-right)
      ("s" sort-lines)
      ("w" whitespace-mode)

      ;; commands to exit hydra-launcher
      ("0" delete-window :color blue)
      ("1" delete-other-windows :color blue)
      ("2" split-window-below :color blue)
      ("3" split-window-right :color blue)

      ("m" org-agenda :color blue)
      ("b" buffer-menu :color blue)
      ("c" org-capture :color blue)
      ("d" hydra-display/body :color blue)
      ("g" magit-status :color blue)
      ("v" helm-bookmarks :color blue)
      ("z" hydra-zoom/body :color blue)
      ("y" hydra-yasnippet/body :color blue)

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

    ;;; Hydra Yasnippet BEGIN
(defhydra hydra-functions (:color blue :hint nil)
  "
        ^Useful Functions^
-----------------------------
 Actions:

 _s_: sort lines    
 _p_: trim right    
 _r_: reload dot emacs

"
  ("p" my-trim-right)
  ("r" my-reload-dot-emacs)
  ("s" sort-lines)
  ("q" nil "cancel" :color blue))
    ;;; Hydra Yasnippet END


    ;;; Hydra Yasnippet BEGIN
(defhydra hydra-yasnippet (:color blue :hint nil)
  "
        ^YASnippets^
-----------------------------
 Actions:

_i_nsert snippet
_v_isit snippet files
_n_ew
_r_eload all

"
  ("i" yas-insert-snippet)
  ("v" yas-visit-snippet-file :color blue)
  ("n" yas-new-snippet)
  ("r" yas-reload-all)
  ("q" nil "cancel" :color blue))
    ;;; Hydra Yasnippet END


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

))
(provide 'init-hydra)
