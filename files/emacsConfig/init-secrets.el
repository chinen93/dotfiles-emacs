;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Secrets
;;
;; DATE_CREATE: 2018-08-24
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'epa-file)
(epa-file-enable)

;; symmetric encryption only
(setq epa-file-select-keys nil)

(setq epg-gpg-program "gpg2")
(setenv "GPG_AGENT_INFO" nil)

(provide 'init-secrets)
