;; emacs init file inspired by drew
;; https://github.com/wernerandrew/drewmacs2/
;; for emacsclient support
;; (server-start)

;; Helper to load files based on relative paths
(defun relative-to-full-path (filename)
    (concat (file-name-directory (or load-file-name buffer-file-name))
            filename))

(load (relative-to-full-path "custom-functions.el"))
;; (load (relative-to-full-path "general-config.el"))
(load (relative-to-full-path "init-packages.el"))
(load (relative-to-full-path "init-org.el"))
(load (relative-to-full-path "init-ui.el"))
(load (relative-to-full-path "init-keys.el"))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes (quote (flatland)))
 '(elpy-modules
   (quote
    (elpy-module-company elpy-module-eldoc elpy-module-flymake elpy-module-pyvenv elpy-module-yasnippet elpy-module-django elpy-module-sane-defaults)))
 '(jedi:server-command
   (quote
    ("~/.emacs.d/.python-environments/default/bin/jediepcserver")))
 '(menu-bar-mode nil)
 '(org-agenda-skip-deadline-if-done t)
 '(org-agenda-skip-scheduled-if-done t)
 '(org-deadline-warning-days 10)
 '(projectile-git-command "git ls-files -zc --exclude-standard")
 '(projectile-globally-ignored-directories
   (quote
    (".idea" ".eunit" ".git" ".hg" ".fslckout" ".bzr" "_darcs" ".tox" ".svn" ".stack-work" ".pyc" "__pycache__")))
 '(python-environment-virtualenv
   (quote
    ("virtualenv" "--no-site-packages" "--quiet" "--python" "python3")))
 '(python-shell-interpreter "/usr/bin/ipython3")
 '(python-shell-interpreter-args "--simple-prompt --pprint -i"))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(put 'dired-find-alternate-file 'disabled nil)


;; (with-eval-after-load 'python
      ;; (add-hook 'python-mode-hook (lambda () (setq python-shell-interpreter
                                                   ;; "python3"))))

