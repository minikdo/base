(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

;; Initialise installed packages
;; (setq package-enable-at-startup t)

;; Do not report warning errors 
(setq native-comp-async-report-warnings-errors 'silent) 

;; Truly maximize screen
(setq frame-resize-pixelwise t)

;; Start maximized
(add-to-list 'default-frame-alist '(fullscreen . maximized))

;; No need for titlebar
(modify-frame-parameters nil '((undecorated . t)))

;; Disable garbage collection during the startup time
(setq gc-cons-threshold 536870912
      gc-cons-percentage 0.6)

(add-hook 'after-init-hook (lambda ()
                             ;; restore after startup
                             (setq gc-cons-threshold 16777216
                                   gc-cons-percentage 0.1)))

(setq debug-on-error nil)

;;; early-init.el ends here
