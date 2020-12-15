(package-initialize)

;; ?
;; (defun relative-to-full-path (filename)
    ;; (concat (file-name-directory (or load-file-name buffer-file-name))
            ;; filename))

;; (load (relative-to-full-path "init-org.el"))

;; install debian packages:
;; sudo apt install $(grep -ri 'Debian packages:' ~/.emacs.d/modes | awk -F:  '{print $3}' | tr '\n' ' ')

(load-file "~/.emacs.d/general.el")

;; (load-file "~/.emacs.d/modes/autopair.el")
(load-file "~/.emacs.d/modes/auctex.el")
(load-file "~/.emacs.d/modes/auto-complete.el")
(load-file "~/.emacs.d/modes/multiple-cursors.el")
(load-file "~/.emacs.d/modes/elisp.el")
(load-file "~/.emacs.d/modes/flx-ido.el")
(load-file "~/.emacs.d/modes/git-annex.el")
(load-file "~/.emacs.d/modes/jediserver.el")
(load-file "~/.emacs.d/modes/mail-mode.el")
(load-file "~/.emacs.d/modes/org.el")
(load-file "~/.emacs.d/modes/php-mode.el")
(load-file "~/.emacs.d/modes/projectile.el")
(load-file "~/.emacs.d/modes/elpy.el")
(load-file "~/.emacs.d/modes/python-mode.el")
(load-file "~/.emacs.d/modes/smex.el")
(load-file "~/.emacs.d/modes/web-mode.el")
(load-file "~/.emacs.d/modes/which-key.el")
(load-file "~/.emacs.d/modes/persistent-scratch.el")
;; (load-file "~/.emacs.d/modes/yas.el")

(load-file "~/.emacs.d/modes/themes.el")

;;?
;;(provide '.emacs)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-agenda-skip-deadline-if-done t t)
 '(org-agenda-skip-scheduled-if-done t t)
 '(org-deadline-warning-days 10 t)
 '(package-selected-packages
   (quote
    (multiple-cursors centaur-tabs rainbow-mode persistent-scratch python python-mode python-environment epc emmet-mode which-key web-mode tabbar smex session pyvenv projectile pod-mode php-mode muttrc-mode mutt-alias markdown-mode jedi initsplit htmlize graphviz-dot-mode git-annex folding flx-ido eproject diminish csv-mode clues-theme browse-kill-ring boxquote bm bar-cursor apache-mode)))
 '(scroll-conservatively 1000)
 '(scroll-margin 3))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(isearch-fail ((t (:background "red")))))
