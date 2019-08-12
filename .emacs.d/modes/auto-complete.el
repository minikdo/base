;; Debian packages: elpa-auto-complete

;; RAUL:
;; (require 'auto-complete)
;; (require 'auto-complete-config)

;; (add-to-list 'ac-dictionary-directories "/usr/share/auto-complete/dict/")

;; (ac-config-default)
;; (defun auto-complete-mode-maybe ()
  ;; "No maybe for you. Only AC!"
  ;; (unless (minibufferp (current-buffer))
    ;; (auto-complete-mode 1)))

;; MY:
;; auto-complete
(require 'auto-complete-config)
(ac-config-default)
;; ac menu (optional)
;;(setq ac-show-menu-immediately-on-auto-complete t)
(setq ac-auto-show-menu (* ac-delay 2))
