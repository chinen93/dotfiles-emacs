

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; HYDRA configuration
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require-package 'hydra)

    ;;; Hydra launcher BEGIN
(defhydra hydra-launcher (:color amaranth
				 :hint nil)
  "
 ^Functions^          ^Window^            ^Other Menus^       ^Help^
-^---------^----------^------^------------^-----------^-------^----^-------
 _w_: whitespace      _0_: delete window  ^           ^       _6_: function
 _f_: functions menu  _1_: only 1 window  _b_: buffer         _7_: variable
 ^ ^                  _2_: divide horiz   _v_: bookmark list  _8_: mode
 ^ ^                  _3_: divide vertc   _V_: bookmark set   _9_: view lossage
 ^ ^                  _4_: other window   _y_: YASnippet      _0_: view Messages
 ^ ^                  ^ ^                 _(_: macro
"
  ;; commands to exec in actual buffer
  ("f" hydra-functions/body :color blue)
  ("w" whitespace-mode)

  ;; commands to exit hydra-launcher
  ("0" delete-window :color blue)
  ("1" delete-other-windows :color blue)
  ("2" split-window-below :color blue)
  ("3" split-window-right :color blue)
  ("4" other-window)

  ("(" hydra-macro/body :color blue)
  ("b" buffer-menu :color blue)
  ("y" hydra-yasnippet/body :color blue)
  ("v" bookmark-bmenu-list :color blue)
  ("V" bookmark-set :color blue)

  ("0" view-echo-area-messages :color blue)
  ("6" describe-function :color blue)
  ("7" describe-variable :color blue)
  ("8" describe-mode :color blue)
  ("9" view-lossage :color blue)

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

    ;;; Hydra Functions BEGIN
(defhydra hydra-functions (:color amaranth :hint nil)
  "
        ^Useful Functions^
-^--------^----------------
 ^Actions:^                

 _s_: Sort lines           
 _p_: Trim right           
 _t_: Truncate lines       
 _f_: Fill paragraph
 _k_: Open file
 _l_: Open Terminal in folder

"
  ("p" my-trim-right)

  ("s" sort-lines)
  ("t" toggle-truncate-lines)
  ("f" endless-fill-or-unfill)
  ("k" xah-open-in-external-app :color blue)
  ("l" xah-open-in-terminal :color blue)

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
    ;;; Hydra Functions END

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

    ;;; Hydra Macro END
(defhydra hydra-macro (:color amaranth :hint nil)
  "
 ^Basic^
-^-----^--------------------------------------
 _j_: Create new macro
 _k_: End creation of new macro
 _e_: Execute last macro
 _n_: Insert Counter

"

  ("j" kmacro-start-macro :color blue)
  ("k" kmacro-end-macro :colocr blue)
  ("e" kmacro-end-or-call-macro-repeat)
  ("n" kmacro-insert-counter)
  
  ("q" nil "quit" :color blue))
    ;;; Hydra Macro END

(provide 'init-windows-hydra)
