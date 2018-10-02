;; Global
;; Never, ever use tabs
(setq-default indent-tabs-mode nil)


;; disable backup
(setq backup-inhibited t)

;; Other settings
(setq-default fill-column 80)
(setq auto-fill-mode t)
(setq-default auto-fill-function 'do-auto-fill)

;; Line numbers
(global-linum-mode t)
(set-window-margins nil 1)
(column-number-mode t)
(setq linum-format "%4d ")

;; save cursor position in files
(setq save-place-file "~/.emacs.d/saveplace")
(setq-default save-place t)
(require 'saveplace)

;; Change dictionary to polish
(setq ispell-dictionary "polish")

;; disable scratch message
(setq initial-scratch-message "")
