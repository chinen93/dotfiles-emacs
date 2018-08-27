;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Hydra
;;
;; DATE_CREATE: 2016-06-29
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require-package 'hydra)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    ;;; Hydra Narrow Widen BEGIN
(defhydra hydra-narrow-widen (:color pink)
  "
 ^Narrow Widen^
-^----^-----------------------------------------------------
 _w_: Widen
 _n_: Narrow to Region

"

  ("w" widen "Widen")
  ("n" narrow-to-region "Narrow region")

  ("q" nil "quit" :color blue)
  ("l" hydra-launcher/body "return" :color blue))
    ;;; Hydra Narrow Widen END

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    ;;; Hydra Help BEGIN
(defhydra hydra-help (:color pink)
  "
 ^Help^
-^----^-----------------------------------------------------
 _f_: function
 _v_: variable
 _m_: mode
 _l_: view lossage
 _M_: view Messages

"

;;  Help
;; ----------------------------------------------------------
;;  f: function
;;  v: variable
;;  m: mode
;;  l: view lossage
;;  M: view Messages

  ("M" view-echo-area-messages :color blue)
  ("f" describe-function :color blue)
  ("v" describe-variable :color blue)
  ("m" describe-mode :color blue)
  ("l" view-lossage :color blue)

  ("q" nil "quit" :color blue)
  ("l" hydra-launcher/body "return" :color blue))
    ;;; Hydra Help END

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    ;;; Hydra launcher BEGIN
(defhydra hydra-launcher (:color amaranth :hint nil)
  "
 ^Functions^         ^Window^             ^Other Menus^    ^Hydra Menus^
-^---------^---------^------^-------------^-----------^----^-----------^---
 _w_: whitespace     _0_: delete window   _b_: buffer      _z_: zoom
 _V_: bookmark set   _1_: only 1 window   _v_: bookmarks   _y_: YASnippet
 _p_: trim spaces    _2_: divide horiz    _g_: magit       _h_: help
 ^ ^                 _3_: divide vertc    _l_: ledger      _f_: functions
 _c_: Capture        _4_: other window    _m_: Agenda      _(_: macro
 ^ ^                 ^ ^                  ^ ^              _r_: rectangle

"

  ;; commands to exec in actual buffer
  ("p" (user--clean-buffer))
  ("f" hydra-functions/body :color blue)
  ("w" whitespace-mode)
  ("V" bookmark-set :color blue)

  ;; commands to exit hydra-launcher
  ("0" delete-window :color blue)
  ("1" delete-other-windows :color blue)
  ("2" split-window-below :color blue)
  ("3" split-window-right :color blue)
  ("4" other-window)

  ("c" org-capture :color blue)
  ("m" org-agenda :color blue)

  ;; Other menus
  ("(" hydra-macro/body :color blue)
  ("l" ledger-report :color blue)
  ("b" buffer-menu :color blue)
  ("g" magit-status :color blue)
  ("v" bookmark-bmenu-list :color blue)
  ("y" hydra-yasnippet/body :color blue)
  ("z" hydra-zoom/body :color blue)
  ("N" hydra-narrow-widen/body :color blue)
  ("h" hydra-help/body :color blue)
  ("r" hydra-rectangle/body :color blue)

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    ;;; Hydra functions BEGIN
(defhydra hydra-functions (:color amaranth :hint nil)
  "
        Useful Functions
^--------^------------------^-------^-------------^--------^---------------
^Actions:^                  ^Toggle:^             ^Internet^

_s_: Sort lines             _t_: Truncate lines   _i_: Word of The day
_p_: Trim whitespaces       _f_: Fill paragraph   _g_: Google
_k_: Open file              ^ ^                   _h_: Google Translate
_l_: Open Terminal          ^ ^                   _d_: Define Word
_r_: Query Replace Regexp

"

  ("p" (user--clean-buffer))

  ("r" query-replace-regexp)
  ("s" sort-lines)
  ("t" toggle-truncate-lines)
  ("f" endless-fill-or-unfill)
  ("k" xah-open-in-external-app :color blue)
  ("l" xah-open-in-terminal :color blue)

  ("i" wotd-select :color blue)
  ("g" google-this-ray :color blue)
  ("h" google-translate-smooth-translate :color blue)
  ("d" define-word-at-point :color blue)

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
    ;;; Hydra functions END

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    ;;; Hydra Yasnippet BEGIN
(defhydra hydra-yasnippet (:color blue :hint nil)
  "
        ^YASnippets^
-----------------------------
 Actions:

_i_: insert snippet
_v_: visit snippet files
_n_: new
_r_: reload all

"

  ("i" yas-insert-snippet)
  ("v" yas-visit-snippet-file :color blue)
  ("n" yas-new-snippet)
  ("r" yas-reload-all)
  ("q" nil "cancel" :color blue))
    ;;; Hydra Yasnippet END

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    ;;; Hydra display BEGIN
(defhydra hydra-display (:color pink :hint nil)
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    ;;; Hydra macro BEGIN
(defhydra hydra-macro (:color amaranth :hint nil)
  "
 ^Basic^
-^-----^--------------------------------------
 _j_: Create new macro
 _k_: End creation of new macro
 _e_: Execute last macro
 _n_: Insert Counter
 _h_: Show last macro as elisp

"

  ("j" kmacro-start-macro :color blue)
  ("k" kmacro-end-macro :colocr blue)
  ("e" kmacro-end-or-call-macro-repeat)
  ("n" kmacro-insert-counter)
  ("h" elmacro-show-last-macro :color blue)

  ("q" nil "quit" :color blue))
    ;;; Hydra macro END

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    ;;; Hydra Rectangle BEGIN
(defhydra hydra-rectangle (:color amaranth :hint nil)
  "
 ^Rectangle^
--------------------------------------------
 _m_: mark region
 _k_: kill region
 _y_: yank region

"
  ("m" rectangle-mark-mode nil)
  ("y" yank-rectangle nil)
  ("k" kill-rectangle nil)

  ("<right>" forward-char)     
  ("<left>" backward-char)     
  ("<down>" next-line)         
  ("<up>" previous-line)       
  ("<home>" move-beginning-of-line)
  ("<end>" move-end-of-line)   
  ("<RET>" nil :color blue)    
  ("<ESC>" nil :color blue)    
                               
  ("q" nil "quit" :color blue))
    ;;; Hydra Rectangle END

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    ;;; Hydra Context BEGIN
(defun hydra-context-launcher ()
  "A launcher for hydras based on the current context.

https://dfeich.github.io/www/org-mode/emacs/2018/05/10/context-hydra.html
"
  (interactive)
  (cl-case major-mode
    ('Buffer-menu-mode (hydra-buffer-menu/body))
    ('org-mode (hydra-org/body))
    (t (message "No hydra for this major mode: %s" major-mode))))

(global-set-key (kbd "<f9>") 'hydra-context-launcher)
    ;;; Hydra Context END

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    ;;; Hydra menu buffer BEGIN
(defhydra hydra-buffer-menu (:color pink :hint nil)
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
    ;;; Hydra menu buffer END

    ;;; Hydra Org BEGIN
(defhydra hydra-org (:color amaranth :hint nil)
  "
 ^Org^
--------------------------------------------
 _s_: Store Link
 _l_: Insert Link

"
  ("s" org-store-link nil :color blue)
  ("l" org-insert-link nil  :color blue)

  ("q" nil "quit" :color blue))
    ;;; Hydra Org END

(provide 'init-hydra)
