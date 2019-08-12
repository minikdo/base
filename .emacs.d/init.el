(package-initialize)

;; ?
;; (defun relative-to-full-path (filename)
    ;; (concat (file-name-directory (or load-file-name buffer-file-name))
            ;; filename))

;; (load (relative-to-full-path "init-org.el"))

(load-file "~/.emacs.d/general.el")

;; (load-file "~/.emacs.d/modes/auctex.el")
(load-file "~/.emacs.d/modes/auto-complete.el")
(load-file "~/.emacs.d/modes/flx-ido.el")
(load-file "~/.emacs.d/modes/git-annex.el")
;; (load-file "~/.emacs.d/modes/jediserver.el")
(load-file "~/.emacs.d/modes/mail-mode.el")
(load-file "~/.emacs.d/modes/org.el")
(load-file "~/.emacs.d/modes/php-mode.el")
;; (load-file "~/.emacs.d/modes/projectile.el")
;; (load-file "~/.emacs.d/modes/python-mode.el")
(load-file "~/.emacs.d/modes/smex.el")
;; (load-file "~/.emacs.d/modes/web-mode.el")
(load-file "~/.emacs.d/modes/which-key.el")
;; (load-file "~/.emacs.d/modes/yas.el")

(load-file "~/.emacs.d/modes/themes.el")

;;?
;;(provide '.emacs)

